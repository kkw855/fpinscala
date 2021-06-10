package com.endsoul.fp.scala
package book.coding_problems

import cats._, cats.implicits._

//noinspection DuplicatedCode
object MasteringFp2 extends App {
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

  implicit val monad: Monad[SimpleWriter] = new Monad[SimpleWriter] {
    override def map[A, B](fa: SimpleWriter[A])(f: A => B): SimpleWriter[B] =
      fa.copy(value = f(fa.value))
    override def flatMap[A, B](fa: SimpleWriter[A])(
        f: A => SimpleWriter[B]): SimpleWriter[B] = {
      val res = f(fa.value)
      SimpleWriter(fa.log ++ res.log, res.value)
    }
    override def pure[A](a: A): SimpleWriter[A] = SimpleWriter(Nil, a)

    @annotation.tailrec
    override def tailRecM[A, B](a: A)(
        f: A => SimpleWriter[Either[A, B]]): SimpleWriter[B] = {
      val next = f(a)
      next.value match {
        case Left(a1)     => tailRecM(a1)(f)
        case Right(value) => pure(value)
      }
    }
  }

  import cats.data.Writer
  def addWriter(a: Double, b: Double): Writer[List[String], Double] =
    for {
      _ <- Writer.tell(List(s"Adding $a to $b"))
      res = a + b
      _ <- Writer.tell(List(s"The result of the operation is $res"))
    } yield res
  // WriterT((List(Adding 1.0 to 2.0, The result of the operation is 3.0),3.0))
  println(addWriter(1, 2))

  @annotation.tailrec
  def factorialTailrec(n: Int, accumulator: Int = 1): Int =
    if (n <= 0) accumulator
    else factorialTailrec(n - 1, n * accumulator)
  println(factorialTailrec(5)) // 120

  implicit val simpleWriterFunctor: Functor[SimpleWriter] =
    new Functor[SimpleWriter] {
      override def map[A, B](fa: SimpleWriter[A])(f: A => B): SimpleWriter[B] =
        fa.copy(value = f(fa.value))
    }

  val x = SimpleWriter(Nil, 3)
  // SimpleWriter(List(),6)
  println(x.map(_ * 2))

  type Fx[A] = Either[List[String], A]
  def combineComputations[F[_]: Monad](f1: F[Double],
                                       f2: F[Double]): F[Double] =
    for {
      r1 <- f1
      r2 <- f2
    } yield r1 + r2

  val result = combineComputations[Fx](Monad[Fx].pure(1.0), Monad[Fx].pure(2.0))
  println(result) // Right(3.0)

  def ap[A, B](ff: Fx[A => B])(fa: Fx[A]): Fx[B] = (ff, fa) match {
    case (Right(f), Right(a)) => Right(f(a))
    case (Left(e1), Left(e2)) => Left(e1 ++ e2)
    case (Left(e), _)         => Left(e)
    case (_, Left(e))         => Left(e)
  }

  def zip[A, B](f1: Fx[A], f2: Fx[B]): Fx[(A, B)] =
    ap[B, (A, B)](ap[A, B => (A, B)](Right { (a: A) => (b: B) =>
      (a, b)
    })(f1))(f2)

//  def zip[A, B](f1: Fx[A], f2: Fx[B]): Fx[(A, B)] = (f1, f2) match {
//    case (Right(r1), Right(r2)) => Right((r1, r2))
//    case (Left(e1), Left(e2))   => Left(e1 ++ e2)
//    case (Left(e), _)           => Left(e)
//    case (_, Left(e))           => Left(e)
//  }

  implicit val applicative: Applicative[Fx] = new Applicative[Fx] {
    override def ap[A, B](ff: Fx[A => B])(fa: Fx[A]): Fx[B] = (ff, fa) match {
      case (Right(f), Right(a)) => Right(f(a))
      case (Left(e1), Left(e2)) => Left(e1 ++ e2)
      case (Left(e), _)         => Left(e)
      case (_, Left(e))         => Left(e)
    }
    override def pure[A](a: A): Fx[A] = Right(a)
  }

  def combineComputations(f1: Fx[Double], f2: Fx[Double]): Fx[Double] =
    zip(f1, f2).map { case (r1, r2) => r1 + r2 }

  val result2 = combineComputations(Monad[Fx].pure(1.0), Monad[Fx].pure(2.0))
  println(result2) // Right(3.0)

  val resultFirstFailed =
    combineComputations(Left(List("Division by zero")), Monad[Fx].pure(2.0))
  println(resultFirstFailed) // Left(List(Division by zero))

  val resultSecondFailed = combineComputations(
    Monad[Fx].pure(1.0),
    Left(List("Null pointer encountered")))
  println(resultSecondFailed) // Left(List(Null pointer encountered))

  val resultBothFailed = combineComputations(
    Left(List("Division by zero")),
    Left(List("Null pointer encountered")))
  println(resultBothFailed) // Left(List(Division by zero, Null pointer encountered))
}
