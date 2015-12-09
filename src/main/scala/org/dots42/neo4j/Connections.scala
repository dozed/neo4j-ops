package org.dots42.neo4j

import org.dots42.Data.ErrorCode
import org.neo4j.graphdb._

import scala.collection.JavaConversions._
import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task

object Connections {

  type ResultItem = Map[String, AnyRef]

  type Params = Map[String, AnyRef]


  case class Connection(db: GraphDatabaseService) {
    var transaction: Option[Transaction] = None
  }

  // connection AST
  sealed trait ConnectionOp[A] {
    def run(c: Connection): A
  }

  case class RunQuery(text: String, params: Params) extends ConnectionOp[Result] {
    override def run(c: Connection): Result = c.db.execute(text, params)
  }

  case class StartTx() extends ConnectionOp[Unit] {
    override def run(c: Connection): Unit = {
      println(f"StartTx  running in thread: ${java.lang.Thread.currentThread().getId}")
      c.transaction = Some(c.db.beginTx())
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
      c.transaction = None
      txOpt.foreach { tx =>
        tx.close()
      }
    }
  }


  // free/operational monad over AST
  type ConnectionIO[A] = Free.FreeC[ConnectionOp, A]

  implicit val MonadConnectionIO: Monad[ConnectionIO] = Free.freeMonad[Coyoneda[ConnectionOp, ?]]


  type ConnectionEither[E, A] = EitherT[ConnectionIO[?], E, A]
  type ConnectionWithError[A] = EitherT[ConnectionIO[?], ErrorCode, A]

  implicit class ConnectionIOExt[A](c: ConnectionIO[Option[A]]) {
    def \/>[E](e: => ConnectionIO[E]): ConnectionEither[E, A] = EitherT[ConnectionIO[?], E, A] {
      c flatMap { x =>
        x match {
          case Some(v) => v.right.point[ConnectionIO]
          case None => e.map(_.left)
        }
      }
    }
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

    def transact(con: Connection): Task[A] = {
      val p = for {
        _ <- startTx
        r <- c
        _ <- successTx
        _ <- closeTx
      } yield r

      Task {
        Free.runFC[ConnectionOp, Reader[Connection, ?], A](p)(interp).run(con)
      }
    }
  }

  // just print the ConnectionOps of the ConnectionIO
  val printInterpreter: ConnectionIO ~> Id.Id = new (ConnectionIO ~> Id.Id) {
    override def apply[A](fa: ConnectionIO[A]): Id.Id[A] = ???
  }

  // ConnectionIO[Result] constructor
  def runQuery(text: String, params: Params): ConnectionIO[org.neo4j.graphdb.Result] = Free.liftFC(RunQuery(text, params))
  def startTx: ConnectionIO[Unit] = Free.liftFC(StartTx())
  def successTx: ConnectionIO[Unit] = Free.liftFC(SuccessTx())
  def failureTx: ConnectionIO[Unit] = Free.liftFC(FailureTx())
  def closeTx: ConnectionIO[Unit] = Free.liftFC(CloseTx())

}





