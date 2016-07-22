package org.dots42.neo4j

import org.neo4j.graphdb._
import org.slf4j.LoggerFactory

import scala.collection.JavaConversions._
import scalaz._
import Scalaz._
import scalaz.concurrent.Task

object Connections {

  type ResultItem = Map[String, Any]

  type Params = Map[String, Any]

  val log = LoggerFactory.getLogger("org.dots42.neo4j.Connection")

  case class Connection(db: GraphDatabaseService) {
    var transaction: Option[Transaction] = None
    var transactionStart: Long = 0L
    var queryCount: Int = 0
  }

  // connection AST
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

  case class StartTx() extends ConnectionOp[Unit] {
    override def run(c: Connection): Unit = {
      log.debug(f"StartTx (thread: ${java.lang.Thread.currentThread().getId})")
      c.transaction = Some(c.db.beginTx())
      c.transactionStart = System.currentTimeMillis()
    }
  }

  case class SuccessTx() extends ConnectionOp[Unit] {
    override def run(c: Connection): Unit = {
      log.debug(f"SuccessTx (thread: ${java.lang.Thread.currentThread().getId})")
      c.transaction.foreach { tx =>
        tx.success()
      }
    }
  }

  case class FailureTx() extends ConnectionOp[Unit] {
    override def run(c: Connection): Unit = {
      log.debug(f"FailureTx (thread: ${java.lang.Thread.currentThread().getId})")
      c.transaction.foreach { tx =>
        tx.failure()
      }
    }
  }

  case class CloseTx() extends ConnectionOp[Unit] {
    override def run(c: Connection): Unit = {
      val txOpt = c.transaction
      val queryCount = c.queryCount
      val deltaSec = (System.currentTimeMillis() - c.transactionStart).toDouble / 1000.0
      log.debug(f"CloseTx (thread: ${java.lang.Thread.currentThread().getId}, qps: $queryCount, time: $deltaSec)")
      c.transaction = None
      c.queryCount = 0
      c.transactionStart = 0L
      txOpt.foreach { tx =>
        tx.close()
      }
    }
  }


  // free/operational monad over AST
  type ConnectionIO[A] = Free[ConnectionOp, A]
  implicit val MonadConnectionIO: Monad[ConnectionIO] = Free.freeMonad[ConnectionOp]


  type ConnectionEither[E, A] = EitherT[ConnectionIO[?], E, A]

  implicit class ConnectionIOExt[A](c: ConnectionIO[A]) {

    def >>[B](c2: ConnectionIO[B]): ConnectionIO[B] = c flatMap (_ => c2)

    // a ConnectionIO[Option[A]] and ConnectionIO[E] as EitherT[ConnectionIO[?], E, A]
    def \/>[B, E](e: => ConnectionIO[E])(implicit ev: A <:< Option[B]): ConnectionEither[E, B] = EitherT[ConnectionIO[?], E, B] {
      c flatMap { x =>
        ev(x) match {
          case Some(v) => v.right.point[ConnectionIO]
          case None => e.map(_.left)
        }
      }
    }


    // a ConnectionIO[Option[A]] as OptionT[ConnectionIO[?], A]
    def asOptT[B](implicit ev: A <:< Option[B]): OptionT[ConnectionIO[?], B] = OptionT[ConnectionIO[?], B](c map ev.apply)

    // a ConnectionIO[A] as OptionT[ConnectionIO[?], A]
    def liftOptT: OptionT[ConnectionIO[?], A] = OptionT[ConnectionIO[?], A](c map (_.some))

  }


  val interp: ConnectionOp ~> Reader[Connection, ?] = new (ConnectionOp ~> Reader[Connection, ?]) {
    override def apply[A](a: ConnectionOp[A]): Reader[Connection, A] = {
      Reader { con => a.run(con) }
    }
  }

  implicit class ConnectionIOExt2[A](c: ConnectionIO[A]) {
    def right[E]: ConnectionEither[E, A] = EitherT[ConnectionIO[?], E, A] {
      c.map(x => x.right)
    }

    def transact: Reader[Connection, A] = {
      val p: ConnectionIO[A] = for {
        _ <- startTx
        r <- c
        _ <- successTx
        _ <- closeTx
      } yield r

      p.foldMap[Reader[Connection, ?]](interp)
    }

    def task(con: Connection): Task[A] = Task(c.transact(con))

  }

  // just print the ConnectionOps of the ConnectionIO
  val printInterpreter: ConnectionIO ~> Id.Id = new (ConnectionIO ~> Id.Id) {
    override def apply[A](fa: ConnectionIO[A]): Id.Id[A] = ???
  }

  // ConnectionIO[Result] constructor
  def runQuery(text: String, params: Params): ConnectionIO[org.neo4j.graphdb.Result] = Free.liftF(RunQuery(text, params))
  def startTx: ConnectionIO[Unit]   = Free.liftF(StartTx())
  def successTx: ConnectionIO[Unit] = Free.liftF(SuccessTx())
  def failureTx: ConnectionIO[Unit] = Free.liftF(FailureTx())
  def closeTx: ConnectionIO[Unit]   = Free.liftF(CloseTx())

}
