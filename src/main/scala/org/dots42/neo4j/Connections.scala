package org.dots42.neo4j

import java.util.logging.Logger

import org.neo4j.driver.v1.Session
import org.neo4j.driver.v1.StatementResult
import org.neo4j.driver.v1.Transaction

import scala.collection.JavaConversions._
import scalaz._
import Scalaz._
import scalaz.concurrent.Task

trait ConnectionTypes {

  type ResultItem = Map[String, Any]

  type Params = Map[String, Any]


  case class Connection(session: Session) {
    var transaction: Option[Transaction] = None
    var transactionStart: Long = 0L
    var queryCount: Int = 0
    val log = Logger.getLogger("Connection")
  }

  // connection AST
  sealed trait ConnectionOp[A] {
    def run(c: Connection): A
  }

  case class RunQuery(text: String, params: Params) extends ConnectionOp[StatementResult] {
    override def run(c: Connection): StatementResult = {
      c.queryCount += 1
      println(s"running query: text=$text params=$params")

      // run either on the session or in the transaction
      c.transaction.fold(c.session.run(text, params.asInstanceOf[Map[String, AnyRef]]))(t => t.run(text, params.asInstanceOf[Map[String, AnyRef]]))

    }
  }

  case class StartTx() extends ConnectionOp[Unit] {
    override def run(c: Connection): Unit = {
      println(f"StartTx  running in thread: ${java.lang.Thread.currentThread().getId}")
      c.transaction = Some(c.session.beginTransaction())
      c.transactionStart = System.currentTimeMillis()
    }
  }

  case class SuccessTx() extends ConnectionOp[Unit] {
    override def run(c: Connection): Unit = {
      println(f"SuccessTx running in thread: ${java.lang.Thread.currentThread().getId}")
      c.transaction.foreach { tx =>
        tx.success()
      }
    }
  }

  case class FailureTx() extends ConnectionOp[Unit] {
    override def run(c: Connection): Unit = {
      println(f"FailureTx running in thread: ${java.lang.Thread.currentThread().getId}")
      c.transaction.foreach { tx =>
        tx.failure()
      }
    }
  }

  case class CloseTx() extends ConnectionOp[Unit] {
    override def run(c: Connection): Unit = {
      println(f"CloseTx running in thread: ${java.lang.Thread.currentThread().getId}")
      val txOpt = c.transaction
      val queryCount = c.queryCount
      val deltaSec = (System.currentTimeMillis() - c.transactionStart).toDouble / 1000.0
      c.transaction = None
      c.queryCount = 0
      c.transactionStart = 0L
      txOpt.foreach { tx =>
        tx.close()
        // println(f"ran $queryCount queries in $deltaSec ms")
      }
    }
  }

  case class CloseSession() extends ConnectionOp[Unit] {
    override def run(c: Connection): Unit = {
      println(f"CloseSession running in thread: ${java.lang.Thread.currentThread().getId}")
      c.session.close
    }
  }
}

trait Connections {

  // free/operational monad over AST
  type ConnectionIO[A] = Free[ConnectionOp, A]
  implicit val MonadConnectionIO: Monad[org.dots42.neo4j.ConnectionIO] = Free.freeMonad[org.dots42.neo4j.ConnectionOp]




  val interp: ConnectionOp ~> Reader[Connection, ?] = new (ConnectionOp ~> Reader[Connection, ?]) {
    override def apply[A](a: ConnectionOp[A]): Reader[Connection, A] = {
      Reader { con => a.run(con) }
    }
  }

  implicit class ConnectionIOExt[A](c: ConnectionIO[A]) {
    def right[E]: ConnectionEither[E, A] = EitherT[ConnectionIO[?], E, A] {
      c.map(x => x.right)
    }

    def transact: Reader[Connection, A] = {
      val p: ConnectionIO[A] = for {
        _ <- startTx
        r <- c
        _ <- successTx
        _ <- closeTx
        _ <- closeSession
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
  def runQuery(text: String, params: Params): ConnectionIO[StatementResult] = Free.liftF(RunQuery(text, params))
  def startTx: ConnectionIO[Unit]   = Free.liftF(StartTx())
  def successTx: ConnectionIO[Unit] = Free.liftF(SuccessTx())
  def failureTx: ConnectionIO[Unit] = Free.liftF(FailureTx())
  def closeTx: ConnectionIO[Unit]   = Free.liftF(CloseTx())
  def closeSession: ConnectionIO[Unit]   = Free.liftF(CloseSession())




  type ConnectionEither[E, A] = EitherT[ConnectionIO[?], E, A]



  implicit class ConnectionIOEitherExt[A](c: ConnectionIO[A]) {

    def right[E]: ConnectionEither[E, A] = EitherT[ConnectionIO[?], E, A] {
      c.map(x => x.right)
    }

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

}
