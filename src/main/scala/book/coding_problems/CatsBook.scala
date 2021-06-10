package com.endsoul.fp.scala
package book.coding_problems

import cats._
import cats.implicits._

import scala.concurrent.Future

case class Result[A](result: A) {
  def foreach(f: A => Unit): Unit = f(result)
  def map[B](f: A => B): Result[B] = Result(f(result))
  def flatMap[B](f: A => Result[B]): Result[B] = f(result)
}

object ForComp extends App {
  val result: Result[Int] = Result(42)
  val anotherResult: Result[Int] = Result(100)

  // foreach
  for {
    res <- result
  } println(res)

  // map
  val map: Result[Int] = for {
    res <- result
  } yield res * 2

  // flatMap
  val flatMap: Result[Int] = for {
    res <- result
    another <- anotherResult
  } yield res + another

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent._
  import scala.concurrent.duration._

  val fm: Monad[Future] = Monad[Future]
  val future: Future[Int] = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))
  Await.result(future, 1.second)

  val optMonad: Option[Int] = 1.pure[Option]

  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    a.flatMap(x => b.map(y => x * x + y * y))

  sumSquare(Option(3), Option(4))
  sumSquare(List(1, 2, 3), List(4, 5))

  def sumSquare2[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x * x + y * y

  sumSquare2(Option(3), Option(4))
  sumSquare2(3: Id[Int], 4: Id[Int])
  // res1: Id[Int] = 25

  val a = Monad[Id].pure(3)
  // a: Id[Int] = 3
  val b = Monad[Id].flatMap(a)(_ + 1)
  // b: Id[Int] = 4

  val either1: Either[String, Int] = Right(10)
  val either2: Either[String, Int] = Right(32)

  for {
    a <- either1
    b <- either2
  } yield a + b
  // res1: Either[String, Int] = Right(42)

  val eitherA = 3.asRight[String]
  // a: Either[String, Int] = Right(3)
  val eitherB = 4.asRight[String]
  // b: Either[String, Int] = Right(4)
  for {
    x <- eitherA
    y <- eitherB
  } yield x * x + y * y
  // res3: Either[String, Int] = Right(25)

  def countPositive(nums: List[Int]) =
    nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
      if (num > 0) {
        accumulator.map(_ + 1)
      } else {
        Left("Negative. Stopping!")
      }
    }
  countPositive(List(1, 2, 3))
  // res5: Either[String, Int] = Right(3)
  countPositive(List(1, -2, 3))
  // res6: Either[String, Int] = Left("Negative. Stopping!")

  Either.catchOnly[NumberFormatException]("foo".toInt)
  // res7: Either[NumberFormatException, Int] = Left(
  //   java.lang.NumberFormatException: For input string: "foo"
  // )
  Either.catchNonFatal(sys.error("Badness"))
  // res8: Either[Throwable, Nothing] = Left(java.lang.RuntimeException: Badness)

  Either.fromTry(scala.util.Try("foo".toInt))
  // res9: Either[Throwable, Int] = Left(
  //   java.lang.NumberFormatException: For input string: "foo"
  // )
  Either.fromOption[String, Int](None, "Badness")
  // res10: Either[String, Int] = Left("Badness")

  "Error".asLeft[Int].getOrElse(0)
  // res11: Int = 0
  "Error".asLeft[Int].orElse(2.asRight[String])
  // res12: Either[String, Int] = Right(2)

  (-1).asRight[String].ensure("Must be non-negative!")(_ > 0)
  // res13: Either[String, Int] = Left("Must be non-negative!")

  "error".asLeft[Int].recover {
    case _: String => -1
  }
  // res14: Either[String, Int] = Right(-1)

  "error".asLeft[Int].recoverWith {
    case _: String => Right(-1)
  }
  // res15: Either[String, Int] = Right(-1)

  "foo".asLeft[Int].leftMap(_.reverse)
  // res16: Either[String, Int] = Left("oof")
  6.asRight[String].bimap(_.reverse, _ * 7)
  // res17: Either[String, Int] = Right(42)
  "bar".asLeft[Int].bimap(_.reverse, _ * 7)
  // res18: Either[String, Int] = Left("rab")

  123.asRight[String]
  // res19: Either[String, Int] = Right(123)
  123.asRight[String].swap
  // res20: Either[Int, String] = Left(123)

  for {
    a <- 1.asRight[String]
    b <- 0.asRight[String]
    c <- if (b == 0) "DIV0".asLeft[Int]
    else (a / b).asRight[String]
  } yield c * 100
  // res21: Either[String, Int] = Left("DIV0")

  sealed trait LoginError extends Product with Serializable
  final case class UserNotFound(username: String) extends LoginError
  final case class PasswordIncorrect(username: String) extends LoginError
  case object UnexpectedError extends LoginError
  case class User(username: String, password: String)
  type LoginResult = Either[LoginError, User]
  def handleError(error: LoginError): Unit =
    error match {
      case UserNotFound(u) =>
        println(s"User not found: $u")

      case PasswordIncorrect(u) =>
        println(s"Password incorrect: $u")

      case UnexpectedError =>
        println(s"Unexpected error")
    }

  val result1: LoginResult = User("dave", "passw0rd").asRight
  // result1: LoginResult = Right(User("dave", "passw0rd"))
  val result2: LoginResult = UserNotFound("dave").asLeft
  // result2: LoginResult = Left(UserNotFound("dave"))

  result1.fold(handleError, println)
  // User(dave,passw0rd)
  result2.fold(handleError, println)
  // User not found: dave

  val x = {
    println("Computing X")
    math.random()
  }
  // Computing X
  // x: Double = 0.15241729989551633

//  x // first access
//  // res0: Double = 0.15241729989551633 // first access
//  x // second access
//  // res1: Double = 0.15241729989551633

  def y = {
    println("Computing Y")
    math.random()
  }
  y // first access
  // Computing Y
  // res2: Double = 0.6963618800921411 // first access
  y // second access
  // Computing Y
  // res3: Double = 0.7321640587866993

  lazy val z = {
    println("Computing Z")
    math.random()
  }

  z // first access
  // Computing Z
  // res4: Double = 0.18457255119783122 // first access
  z // second access
  // res5: Double = 0.18457255119783122

  val eval_x = Eval.now {
    println("Computing X")
    math.random()
  }
  // Computing X
  // x: Eval[Double] = Now(0.681816469770503)

  eval_x.value // first access
  // res10: Double = 0.681816469770503 // first access
  eval_x.value // second access
  // res11: Double = 0.681816469770503

  val eval_y = Eval.always {
    println("Computing Y")
    math.random()
  }
  // y: Eval[Double] = cats.Always@414a351

  eval_y.value // first access
  // Computing Y
  // res12: Double = 0.09982997820703643 // first access
  eval_y.value // second access
  // Computing Y
  // res13: Double = 0.34240334819463436

  val eval_z = Eval.later {
    println("Computing Z")
    math.random()
  }
  // z: Eval[Double] = cats.Later@b0a344a

  eval_z.value // first access
  // Computing Z
  // res14: Double = 0.3604236919233441 // first access
  eval_z.value // second access
  // res15: Double = 0.3604236919233441

  val greeting = Eval
    .always { println("Step 1"); "Hello" }
    .map { str =>
      println("Step 2"); s"$str world"
    }
  // greeting: Eval[String] = cats.Eval$$anon$4@2319703e

  greeting.value
  // Step 1
  // Step 2
  // res16: String = "Hello world"

  val ans = for {
    a <- Eval.now { println("Calculating A"); 40 }
    b <- Eval.always { println("Calculating B"); 2 }
  } yield {
    println("Adding A and B")
    a + b
  }
  // Calculating A
  // ans: Eval[Int] = cats.Eval$$anon$4@2d0f2cbf

  ans.value // first access
  // Calculating B
  // Adding A and B
  // res17: Int = 42 // first access
  ans.value // second access
  // Calculating B
  // Adding A and B
  // res18: Int = 42

  val saying = Eval
    .always { println("Step 1"); "The cat" }
    .map { str =>
      println("Step 2"); s"$str sat on"
    }
    .memoize
    .map { str =>
      println("Step 3"); s"$str the mat"
    }
  // saying: Eval[String] = cats.Eval$$anon$4@ca01c64

  saying.value // first access
  // Step 1
  // Step 2
  // Step 3
  // res19: String = "The cat sat on the mat" // first access
  saying.value // second access
  // Step 3
  // res20: String = "The cat sat on the mat"

  def factorial(n: BigInt): Eval[BigInt] =
    if (n == 1) {
      Eval.now(n)
    } else {
      Eval.defer(factorial(n - 1).map(_ * n))
    }

  println(factorial(50000).value)
  // res: A very big value
}
