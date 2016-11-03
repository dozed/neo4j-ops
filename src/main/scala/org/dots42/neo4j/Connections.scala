package org.dots42.neo4j

import org.slf4j.LoggerFactory

import scala.collection.JavaConversions._
import scalaz._, Scalaz._

object Connections {

  type Node = org.neo4j.graphdb.Node
  type QueryStatistics = org.neo4j.graphdb.QueryStatistics
  type Relationship = org.neo4j.graphdb.Relationship
  type GraphDatabaseService = org.neo4j.graphdb.GraphDatabaseService
  type Result = org.neo4j.graphdb.Result
  type Transaction = org.neo4j.graphdb.Transaction
  type ResultItem = Map[String, Any]
  type Params = Map[String, Any]

  val log = LoggerFactory.getLogger("org.dots42.neo4j.Connection")

  case class Connection(db: GraphDatabaseService) {
    var transactionStart: Long = 0L
    var queryCount: Int = 0
  }


  // input/output operations on a db connection
  sealed trait ConnectionOp[A] {
    def run(c: Connection): A
  }

  case class RunQuery(text: String, params: Params) extends ConnectionOp[Result] {
    override def run(c: Connection): Result = {
      c.queryCount += 1
      log.debug(s"running query: $text")
      c.db.execute(text, params.asInstanceOf[Map[String, AnyRef]])
    }
  }

  case class BeginTx() extends ConnectionOp[Transaction] {
    override def run(c: Connection): Transaction = {
      val tx = c.db.beginTx()
      log.debug(f"StartTx (thread: ${java.lang.Thread.currentThread().getId})")
      c.transactionStart = System.currentTimeMillis()
      tx
    }
  }

  case class SuccessTx(tx: Transaction) extends ConnectionOp[Unit] {
    override def run(c: Connection): Unit = {
      log.debug(f"SuccessTx (thread: ${java.lang.Thread.currentThread().getId})")
      tx.success()
    }
  }

  case class FailureTx(tx: Transaction) extends ConnectionOp[Unit] {
    override def run(c: Connection): Unit = {
      log.debug(f"FailureTx (thread: ${java.lang.Thread.currentThread().getId})")
      tx.failure()
    }
  }

  case class CloseTx(tx: Transaction) extends ConnectionOp[Unit] {
    override def run(c: Connection): Unit = {
      val queryCount = c.queryCount
      val deltaSec = (System.currentTimeMillis() - c.transactionStart).toDouble / 1000.0
      log.debug(f"CloseTx (thread: ${java.lang.Thread.currentThread().getId}, qps: $queryCount, time: $deltaSec)")
      c.queryCount = 0
      c.transactionStart = 0L
      tx.close()
    }
  }

  case class ConnectionIOError[A](t: Throwable) extends ConnectionOp[A] {
    override def run(c: Connection): A = {
      throw t
    }
  }


  // constructors
  def runQuery(text: String, params: Params): ConnectionIO[org.neo4j.graphdb.Result] = Free.liftF(RunQuery(text, params))
  def beginTx: ConnectionIO[Transaction]   = Free.liftF(BeginTx())
  def successTx(tx: Transaction): ConnectionIO[Unit] = Free.liftF(SuccessTx(tx))
  def failureTx(tx: Transaction): ConnectionIO[Unit] = Free.liftF(FailureTx(tx))
  def closeTx(tx: Transaction): ConnectionIO[Unit]   = Free.liftF(CloseTx(tx))


  // free/operational monad for connection operations
  type ConnectionIO[A] = Free[ConnectionOp, A]

  implicit val MonadConnectionIO: Monad[ConnectionIO] = Free.freeMonad[ConnectionOp]


  object ConnectionIO {

    def delay[A](a: => A): ConnectionIO[A] = Free.liftF[ConnectionOp, A](new ConnectionOp[A] {
      override def run(c: Connection): A = a
    })

    def fail[A](t: Throwable): ConnectionIO[A]   = Free.liftF(ConnectionIOError(t))

  }


  val interp: ConnectionOp ~> Reader[Connection, ?] = new (ConnectionOp ~> Reader[Connection, ?]) {
    override def apply[A](a: ConnectionOp[A]): Reader[Connection, A] = {
      Reader { con => a.run(con) }
    }
  }


  type ConnectionEither[E, A] = EitherT[ConnectionIO[?], E, A]

  implicit class ConnectionIOExt[A](fa: ConnectionIO[A]) {

    def withFilter(p: A => Boolean): ConnectionIO[A] = {
      fa.flatMap { a =>
        if (p(a)) fa
        else ConnectionIO.fail(new RuntimeException("filter"))
      }
    }

    def >>[B](c2: ConnectionIO[B]): ConnectionIO[B] = fa.flatMap(_ => c2)

    def require[B](t: => Throwable)(implicit ev: A <:< Option[B]): ConnectionIO[B] = {
      fa.flatMap { x =>
        ev(x) match {
          case Some(v) => v.point[ConnectionIO]
          case None => ConnectionIO.fail(t)
        }
      }
    }

    def transact: Reader[Connection, A] = {
      val p: ConnectionIO[A] = for {
        tx <- beginTx
        r <- fa
        _ <- successTx(tx)
        _ <- closeTx(tx)
      } yield r

      p.foldMap[Reader[Connection, ?]](interp)
    }

    def attempt: Reader[Connection, Throwable \/ A] = Reader { con =>
      \/.fromTryCatchNonFatal(transact(con))
    }


    // OptionT
    def asOptT[B](implicit ev: A <:< Option[B]): OptionT[ConnectionIO[?], B] = OptionT[ConnectionIO[?], B](fa.map(ev))

    def liftOptT: OptionT[ConnectionIO[?], A] = OptionT[ConnectionIO[?], A](fa.map(_.some))


    // EitherT
    def \/>[B, E](e: => ConnectionIO[E])(implicit ev: A <:< Option[B]): ConnectionEither[E, B] = EitherT[ConnectionIO[?], E, B] {
      fa.flatMap { x =>
        ev(x) match {
          case Some(v) => v.right.point[ConnectionIO]
          case None => e.map(_.left)
        }
      }
    }

    def right[E]: ConnectionEither[E, A] = EitherT[ConnectionIO[?], E, A] {
      fa.map(x => x.right)
    }

  }


}
