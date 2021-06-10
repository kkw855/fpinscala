package com.endsoul.fp.scala
package book.coding_problems

case class Boxed[T](value: T)
case class MyLogged[T](value: T, log: List[String])

abstract class MyOption[+T]
case class MySome[T](x: T) extends MyOption[T]
object MyNone extends MyOption[Nothing]

class Lazy[T](value: () => T) {
  def getValue: T = value()
}

abstract class MyList[+T]
case class Cons[T](h: T, t: MyList[T]) extends MyList[T]
object MyNil extends MyList[Nothing]

object LearnMonad extends App {
  def initBoxed(x: Int): Boxed[Int] = Boxed(x)
  def initLogged(x: Int): MyLogged[Int] = MyLogged(x, List())
  def initMyOption(x: Int): MyOption[Int] = MySome(x)
  def initLazy(x: => Int): Lazy[Int] = new Lazy(() => x)
  def initMyList(x: Int): MyList[Int] = Cons(x, MyNil)

  def double(x: Int): Int = x + x
  def sqrt(x: Int): Int = Math.sqrt(x).toInt

  def doubleBoxed(x: Int): Boxed[Int] = Boxed(x + x)
  def sqrtBoxed(x: Int): Boxed[Int] = Boxed(Math.sqrt(x).toInt)
  def doubleLogged(x: Int): MyLogged[Int] =
    MyLogged(x + x, List(s"double($x) = ${x + x}"))
  def sqrtLogged(x: Int): MyLogged[Int] =
    MyLogged(Math.sqrt(x).toInt,
             List(s"sqrt($x).toInt = ${Math.sqrt(x).toInt}"))
  def doubleMyOption(x: Int): MyOption[Int] = MySome(x + x)
  def sqrtMyOption(x: Int): MyOption[Int] =
    if (x >= 0) MySome(Math.sqrt(x).toInt) else MyNone
  def doubleLazy(x: => Int): Lazy[Int] =
    new Lazy(() => { println(s"lazy double($x) run"); x + x })
  def sqrtLazy(x: => Int): Lazy[Int] =
    new Lazy(() => { println(s"lazy sqrt($x) run"); Math.sqrt(x).toInt })
  def doubleMyList(x: Int): MyList[Int] = Cons(x + x, MyNil)
  def sqrtMyList(x: Int): MyList[Int] = Cons(Math.sqrt(x).toInt, MyNil)

  def o[T, V, U](f: T => V, g: V => U): T => U = (x: T) => g(f(x))

  val doubleThenSqrt = o(double, sqrt)
  println(s"doubleThenSqrt: ${doubleThenSqrt(8)}")

  def o2[T, V, U](f: T => V): (V => U) => T => U =
    (g: V => U) => (x: T) => g(f(x))

  val doubleThenSqrt2 = o2(double)(sqrt)
  println(s"doubleThenSqrt2: ${doubleThenSqrt2(8)}")

  def mkBoxedFun(f: Int => Boxed[Int]): Boxed[Int] => Boxed[Int] = x => {
    val value = x.value
    val value2 = f(value)
    value2
  }

  val x1: MyList[Int] = Cons(1, Cons(2, Cons(8, MyNil)))

  val x2 = o(mkBoxedFun(doubleBoxed), mkBoxedFun(sqrtBoxed))(initBoxed(8))
  println(
    s"o(mkBoxedFun(doubleBoxed), mkBoxedFun(sqrtBoxed))(initBoxed(8)): $x2")

  def mkLoggedFun(f: Int => MyLogged[Int]) = (x: MyLogged[Int]) => {
    val value = x.value
    val value2 = f(value)
    value2
  }

  val x3 = o(mkLoggedFun(doubleLogged), mkLoggedFun(sqrtLogged))(initLogged(8))
  println(
    s"o(mkLoggedFun(doubleLogged), mkLoggedFun(sqrtLogged))(initLogged(8)): $x3")

  def mkLoggedFunRevised(f: Int => MyLogged[Int]) = (x: MyLogged[Int]) => {
    val value = x.value
    val log = x.log
    val value2 = f(value)
    MyLogged(value2.value, log ::: value2.log)
  }

  val x4 = o(mkLoggedFunRevised(doubleLogged), mkLoggedFunRevised(sqrtLogged))(
    initLogged(8))
  println(
    s"o(mkLoggedFunRevised(doubleLogged), mkLoggedFunRevised(sqrtLogged))(initLogged(8)): $x4")

  def mkMyOptionFun(f: Int => MyOption[Int]) =
    (x: MyOption[Int]) =>
      x match {
        case MyNone    => MyNone
        case MySome(x) => f(x)
    }

  val x5 = o(mkMyOptionFun(doubleMyOption), mkMyOptionFun(sqrtMyOption))(
    initMyOption(8))
  println(s"o(mkMyOptionFun(doubleMyOption), mkMyOptionFun(sqrtMyOption)): $x5")

  val errVal =
    o(mkMyOptionFun(doubleMyOption), mkMyOptionFun(sqrtMyOption))(MySome(-8))
  println(
    s"o(mkMyOptionFun(doubleMyOption), mkMyOptionFun(sqrtMyOption))(MySome(-8)): $errVal")

  def mkLazyFun(f: Int => Lazy[Int]) = (x: Lazy[Int]) => {
    def value = x.getValue
    def tmpFun() = {
      val y = f(value)
      y.getValue
    }
    new Lazy(tmpFun)
  }

  val x6 = o(mkLazyFun((x) => doubleLazy(x)), mkLazyFun((x) => sqrtLazy(x)))(
    initLazy(8))
  println(
    s"o(mkLazyFun(doubleLazy), mkLazyFun(sqrtLazy))(initLazy(8)): ${x6.getValue}")

  def mkMyListFun(f: Int => MyList[Int]) = (x: MyList[Int]) => {
    def append(l1: MyList[Int], l2: MyList[Int]): MyList[Int] = l1 match {
      case Cons(h, t) => Cons(h, append(t, l2))
      case MyNil      => l2
    }

    def mapAll(l: MyList[Int]): MyList[Int] = l match {
      case Cons(h, t) => {
        val value2 = f(h)
        val remain = mapAll(t)
        append(value2, remain)
      }
      case MyNil => MyNil
    }

    mapAll(x)
  }

  // x1 = Cons(1,Cons(2,Cons(8,MyNil)))
  val x7 = o(mkMyListFun(doubleMyList), mkMyListFun(sqrtMyList))(x1)
  println(s"o(mkMyListFun(doubleMyList), mkMyListFun(sqrtMyList))(x1): $x7")
}
