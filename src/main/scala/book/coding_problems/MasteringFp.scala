package com.endsoul.fp.scala
package book.coding_problems

import java.util.concurrent.Executors
import scala.concurrent.{
  ExecutionContext,
  ExecutionContextExecutorService,
  Future
}
import scala.util.{Failure, Success, Try}

//noinspection DuplicatedCode,ScalaUnusedSymbol,ConvertExpressionToSAM,NotImplementedCode
object MasteringFp extends App {
  def divide(n1: Double, n2: Double): Try[Double] =
    if (n2 == 0) Failure(new RuntimeException("Division by zero!"))
    else Success(n1 / n2)

  def f1(x: Double): Try[Double] =
    divide(2, x) match {
      case Success(res)   => Success(res + 3)
      case f @ Failure(_) => f
    }

  def f1Map(x: Double): Try[Double] =
    divide(2, x).map(r => r + 3)

  def f2Match(x: Double, y: Double): Try[Double] =
    divide(2, x) match {
      case Success(r1) =>
        divide(r1, y) match {
          case Success(r2)    => Success(r2 + 3)
          case f @ Failure(_) => f
        }
      case f @ Failure(_) => f
    }

  def f2FlatMap(x: Double, y: Double): Try[Double] =
    divide(2, x).flatMap(r1 => divide(r1, y)).map(r2 => r2 + 3)

  class Dummy(val id: Int) {
    val f: Int => String = x => s"Number: $x; Dummy: $id"
  }

  val f1: Dummy => Int => String = d => x => s"Number: $x; Dummy: ${d.id}"
  println(f1(new Dummy(1))(2))

  val f2: Int => Dummy => String = x => d => s"Number: $x; Dummy: ${d.id}"
  println(f2(2)(new Dummy(1)))

  case class Event(time: Long, location: String)
  // Retrieve an event from the database
  def getEvent(id: Int): Event = {
    Thread.sleep(1000)
    Event(System.currentTimeMillis(), "New York")
  }
  // Contact a weather forecast server
  def getWeather(time: Long, location: String): String = {
    Thread.sleep(1000)
    "bad"
  }
  // If the weather is bad, we can send a notification to the user
  def notifyUser(): Unit = {
    Thread.sleep(1000)
    println("The user is notified")
  }

  def weatherImperative(eventId: Int): Unit = {
    val evt = getEvent(eventId)
    val weather = getWeather(evt.time, evt.location)
    if (weather == "bad") notifyUser()
  }

  // run every computation in a separate thread
  def thread(op: => Unit): Thread =
    new Thread(new Runnable {
      override def run(): Unit = op
    })
  def runThread(t: Thread): Unit = t.start()

  def notifyThread(weather: String) = thread {
    if (weather == "bad") notifyUser()
  }
  def weatherThread(evt: Event): Thread = thread {
    val weather = getWeather(evt.time, evt.location)
    runThread(notifyThread(weather))
  }
  def eventThread(id: Int): Thread = thread {
    val evt = getEvent(id)
    runThread(weatherThread(evt))
  }
  // Run the app
  runThread(eventThread(10))

  // the callback pattern
  def weatherFuture(eventId: Int): Unit = {
    implicit val context: ExecutionContextExecutorService =
      ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(5))

    Future { getEvent(10) }.onComplete {
      case Success(evt) =>
        Future {
          getWeather(evt.time, evt.location)
        }.onComplete {
          case Success(weather) if weather == "bad" => Future { notifyUser() }
          case _                                    =>
        }
      case _ =>
    }
  }
  weatherFuture(10)

  def weatherFutureFlatmap(eventId: Int): Future[Unit] = {
    implicit val context: ExecutionContextExecutorService =
      ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(5))

    for {
      evt <- Future(getEvent(eventId))
      weather <- Future(getWeather(evt.time, evt.location))
      _ <- Future { if (weather == "bad") notifyUser() }
    } yield ()
  }
  weatherFutureFlatmap(10)

  def weatherFutureFlatmapDesugared(eventId: Int): Future[Unit] = {
    implicit val context: ExecutionContextExecutorService =
      ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(5))
    Future { getEvent(eventId) }
      .flatMap { evt =>
        Future { getWeather(evt.time, evt.location) }
      }
      .flatMap { weather =>
        Future { if (weather == "bad") notifyUser() }
      }
  }

  def division(n1: Double, n2: Double): Either[String, Double] =
    if (n2 == 0) Left("Division by zero!")
    else Right(n1 / n2)

  // Left(Division by zero!)
  println(division(1, 0))
  // Right(1.0)
  println(division(2, 2))

  class Connection
  case class User(id: Option[Int], name: String)
  case class Account(id: Option[Int], ownerId: Int, balance: Double)

  def createUser(u: User, c: Connection): Int = ???
  def createAccount(a: Account, c: Connection): Int = ???
  def registerNewUser(name: String, c: Connection): Int = {
    val uid = createUser(User(None, name), c)
    val accId = createAccount(Account(None, uid, 0), c)
    accId
  }

  def createUserFunc(u: User): Connection => Int = ???
  def createAccountFunc(a: Account): Connection => Int = ???
  def registerNewUserFunc(name: String): Connection => Int = c => {
    val uid = createUserFunc(User(None, name))(c)
    val accId = createAccountFunc(Account(None, uid, 0))(c)
    accId
  }

  case class Reader[A, B](f: A => B) {
    def apply(a: A): B = f(a)
    def flatMap[C](f2: B => Reader[A, C]): Reader[A, C] =
      Reader { a =>
        f2(f(a))(a)
      }
  }

  def createUserReader(u: User): Reader[Connection, Int] = Reader { _ =>
    0
  }
  def createAccountReader(a: Account): Reader[Connection, Int] = Reader { _ =>
    1
  }
  def registerNewUserReader(name: String): Reader[Connection, Int] =
    createUserReader(User(None, name)).flatMap(uid =>
      createAccountReader(Account(None, uid, 0)))

  val reader = registerNewUserReader("John")
  val accId = reader(new Connection)
  // Success, account id: 1
  println(s"Success, account id: $accId")

  case class FullUser(name: String, id: Int, passwordHash: String)
  case class ShortUser(name: String, id: Int)

  implicit def full2short(u: FullUser): ShortUser =
    ShortUser(u.name, u.id)
  def respondWith(user: ShortUser): Unit = ???
  val rootUser = FullUser("root", 0, "acbd18db4cc2f85cedef654fccc4a4d8")
  val handlerExplicit: PartialFunction[String, Unit] = {
    case "/root_user" => respondWith(full2short(rootUser))
  }
  val handlerImplicit: PartialFunction[String, Unit] = {
    case "/root_user" => respondWith(rootUser)
  }

  //  implicit def augmentString(x: String): StringOps
  println("Foo".filter(_ != 'o'))

  case class SimpleWriter[A](log: List[String], value: A) {
    def flatMap[B](f: A => SimpleWriter[B]): SimpleWriter[B] = {
      val wb: SimpleWriter[B] = f(value)
      SimpleWriter(log ++ wb.log, wb.value)
    }
    def map[B](f: A => B): SimpleWriter[B] =
      SimpleWriter(log, f(value))
  }

  object SimpleWriter {
    def pure[A](value: A): SimpleWriter[A] = SimpleWriter(Nil, value)
    def log(message: String): SimpleWriter[Unit] =
      SimpleWriter(List(message), ())
  }

  def addSimpleWriter(a: Double, b: Double): SimpleWriter[Double] = {
    import SimpleWriter.log
    for {
      _ <- log(s"Adding $a to $b")
      res = a + b
      _ <- log(s"The result of the operation is $res")
    } yield res
  }

  // SimpleWriter(List(Adding 1.0 to 2.0, The result of the operation is 3.0),3.0)
  println(addSimpleWriter(1, 2))

  case class IO[A](operation: () => A) {
    def flatMap[B](f: A => IO[B]): IO[B] =
      IO.suspend { f(operation()).operation() }
    def map[B](f: A => B): IO[B] =
      IO.suspend { f(operation()) }
  }
  object IO {
    def suspend[A](op: => A): IO[A] = IO(() => op)
    def log(str: String): IO[Unit] =
      IO.suspend { println(s"Writing message to log file: $str") }
  }
  def addIO(a: Double, b: Double): IO[Double] = {
    import IO.log
    for {
      _ <- log(s"Adding $a to $b")
      res = a + b
      _ <- log(s"The result of the operation is $res")
    } yield res
  }

  // Outputs:
  // Writing message to log file: Adding 1.0 to 2.0
  // Writing message to log file: The result of the operation is 3.0
  addIO(1, 2).operation()

  trait Monad[F[_]] {
    def pure[A](a: A): F[A]
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }
  object Monad {
    implicit class Ops[F[_], A](fa: F[A])(implicit m: Monad[F]) {
      def map[B](f: A => B): F[B] = m.map(fa)(f)
      def flatMap[B](f: A => F[B]): F[B] = m.flatMap(fa)(f)
    }
    implicit val writerMonad: Monad[SimpleWriter] = new Monad[SimpleWriter] {
      def pure[A](a: A): SimpleWriter[A] = SimpleWriter.pure(a)
      def map[A, B](fa: SimpleWriter[A])(f: A => B): SimpleWriter[B] = fa.map(f)
      def flatMap[A, B](fa: SimpleWriter[A])(
          f: A => SimpleWriter[B]): SimpleWriter[B] = fa.flatMap(f)
    }
    implicit val ioMonad: Monad[IO] = new Monad[IO] {
      def pure[A](a: A): IO[A] = IO.suspend(a)
      def map[A, B](fa: IO[A])(f: A => B): IO[B] = fa.map(f)
      def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
    }
  }

  trait Logging[F[_]] {
    def log(msg: String): F[Unit]
  }
  object Logging {
    implicit val writerLogging: Logging[SimpleWriter] =
      new Logging[SimpleWriter] {
        def log(msg: String): SimpleWriter[Unit] = SimpleWriter.log(msg)
      }
    implicit val ioLogging: Logging[IO] = new Logging[IO] {
      def log(msg: String): IO[Unit] = IO.log(msg)
    }
  }

//  def add[F[_]](a: Double, b: Double): F[Double] =
//    for {
//      _ <- log(s"Adding $a to $b")
//      res = a + b
//      _ <- log(s"The result of the operation is $res")
//    } yield res
  import Monad.Ops
  def add[F[_]](a: Double, b: Double)(implicit M: Monad[F],
                                      L: Logging[F]): F[Double] =
    for {
      _ <- L.log(s"Adding $a to $b")
      res = a + b
      _ <- L.log(s"The result of the operation is $res")
    } yield res

  // SimpleWriter(List(Adding 1.0 to 2.0, The result of the operation is 3.0),3.0)
  println(add[SimpleWriter](1, 2))
  // Outputs:
  // Writing message to log file: Adding 1.0 to 2.0
  // Writing message to log file: The result of the operation is 3.0
  // 3.0
  println(add[IO](1, 2).operation())
}
