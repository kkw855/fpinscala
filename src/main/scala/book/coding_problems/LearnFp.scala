package com.endsoul.fp.scala
package book.coding_problems

import scala.annotation.tailrec

//noinspection DuplicatedCode,NotImplementedCode,ScalaUnusedSymbol
object LearnFp extends App {
  object Functions {
    def method(name: String): (Int, String) => String = {
      // name 변수 커링
      def function(in1: Int, in2: String): String = name + in2
      function
    }
  }

  val func: (Int, String) => String = Functions.method("Hello")
  // Hello World!
  println(func(1000, " World!"))

  val next: (Double, Double) => Double = Math.nextAfter
  println(next(1.0, 2.0))

  def four(one: String, two: Int, three: Boolean, four: Long): Unit = ()
  // "one", true 커링
  val applyTwo: (Int, Long) => Unit = four("one", _, true, _)

  // 4가지 함수 리터럴 정의 방법
  val hash: (Int, Boolean, String, Long) => Int = (a, b, c, d) => {
    val ab = 31 * a.hashCode() + b.hashCode()
    val abc = 31 * ab + c.hashCode
    31 * abc + d.hashCode()
  }

  val hashInferred = (a: Int, b: Boolean, c: String, d: Long) => {
    val ab = 31 * a.hashCode() + b.hashCode()
    val abc = 31 * ab + c.hashCode
    31 * abc + d.hashCode()
  }

  def printHash(hasher: String => Int)(s: String): Unit =
    println(hasher(s))

  val hasher1: String => Int = s => s.hashCode
  val hasher2 = (s: String) => s.hashCode
  printHash(hasher1)("Full")
  printHash(hasher2)("Inferred result type")

  sealed trait Glass[+Contents]
  case class Full[Contents](c: Contents) extends Glass[Contents]
  case object EmptyGlass extends Glass[Nothing]
  case class Water(purity: Int)

  def drink(glass: Glass[Water]): Unit = ???
  def drinkAndRefill[C](glass: Glass[C]): Glass[C] = glass
  def drinkAndRefillWater[B >: String, C >: B](glass: Glass[B]): Glass[C] =
    glass

  val glassInt: Glass[Int] = Full(10)
  val glassEmpty: Glass[Int] = EmptyGlass

  drinkAndRefill(Full("Full"))
  drinkAndRefill(Full(Water(1)))

  def printer[A, B, C <: A](a: C)(f: A => B): Unit = println(f(a))

  printer("HoHo")(_.length)
  printer(42)(_ * 20)

  def reverse(s: String): String =
    if (s.length < 2) s
    else reverse(s.tail) + s.head

  println(reverse("Recursive function call"))

  def tailRecReverse(s: String): String = {
    @tailrec
    def reverse(s: String, acc: String): String = {
      if (s.length < 2) s + acc
      else reverse(s.tail, acc.prepended(s.head))
    }
    reverse(s, "")
  }

  println(tailRecReverse("Recursive function call"))

  import util.control.TailCalls._

  def tailA(m: BigInt, n: BigInt): TailRec[BigInt] = {
    if (m == 0) done(n + 1)
    else if (n == 0) tailcall(tailA(m - 1, 1))
    else tailcall(tailA(m, n - 1)).flatMap(tailA(m - 1, _))
  }
  def A(m: Int, n: Int): BigInt = tailA(m, n).result

  val doReverse: PartialFunction[String, String] = {
    case str if str.length > 4 => str.reverse
  }
  val noReverse: PartialFunction[String, String] = {
    case str if str.length <= 4 => str
  }
  def reversePartial = doReverse orElse noReverse
  println(reversePartial("abcd"))

  val nullOpt: Option[String] = Option(null)
  println(nullOpt)

  val upper = (_: String).toUpperCase
  println(upper("abc"))

  def fill(c: Char) = c.toString * (_: String).length
  val aFill = fill('a')
  val bFill = fill('b')
  println(aFill("ab"))
  println(bFill("abc"))

  def filter(c: Char) = (_: String).filter(_ == c)
  println(filter('a')("abca"))

  val chain = List(upper, filter('L'), fill('*'))
  // 아규먼트와 리턴 타입이 같은 함수만 composition 가능
  val allAtOnce = Function.chain(chain)
  println(allAtOnce("Parallel"))

  // previous 리턴 타입과 next 의 아규먼트가 같아야함
  val next2 = (_: String) => 200
  val next3 = (_: String) => "Hello"
  upper andThen next2

  val func2: Function2[Double, Double, Double] = (a: Double, b: Double) => a + b
  println(func2.toString)

  case class A[T](a: T)
  case class B[T](b: T)
  implicit def a2A[T](a: T): A[T] = A(a)
  implicit def b2B[T](a: T): B[T] = B(a)
  def ab[C](a: B[A[C]]): Unit = println(a)

  println(ab(A("A")))
//  ab("A")

}
