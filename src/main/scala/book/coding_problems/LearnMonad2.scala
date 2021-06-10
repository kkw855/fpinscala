package com.endsoul.fp.scala
package book.coding_problems

case class Logged[T](value: T, log: List[String]) {
  def bind(f: T => Logged[T]): Logged[T] = {
    val value2 = f(value)
    val log2 = log ::: value2.log
    Logged(value2.value, log2)
  }
}

object Logged {
  def unit[T](x: T): MyLogged[T] = MyLogged(x, List())
}

object LearnMonad2 extends App {
  def doubleLogged(x: Int): Logged[Int] =
    Logged(x + x, List(s"double($x) = ${x + x}"))
  def sqrtLogged(x: Int): Logged[Int] =
    Logged(Math.sqrt(x).toInt, List(s"sqrt($x).toInt = ${Math.sqrt(x).toInt}"))

  val x1 = doubleLogged(10) bind sqrtLogged
  println(s"doubleLogged(10) bind sqrtLogged: $x1")
}
