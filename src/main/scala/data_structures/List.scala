package com.endsoul.fp.scala
package data_structures

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil        => 0
    case Cons(h, t) => h + sum(t)
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Nil        => Nil
    case Cons(_, t) => t
  }

  def setHead[A](as: List[A], a: A): List[A] = as match {
    case Nil        => Nil
    case Cons(_, t) => Cons(a, t)
  }

  @tailrec
  def drop[A](as: List[A], n: Int): List[A] =
    if (n == 0) as
    else
      as match {
        case Nil        => Nil
        case Cons(_, t) => drop(t, n - 1)
      }

  @tailrec
  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil                => Nil
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _                  => as
  }

  def init[A](as: List[A]): List[A] = as match {
    case Nil                    => Nil
    case Cons(_, t) if t == Nil => Nil
    case Cons(h, t)             => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil        => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def product2(as: List[Double]): Double =
    foldRight(as, 1.0)(_ * _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, b) => 1 + b)

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum3(as: List[Int]): Int =
    foldLeft(as, 0)(_ + _)

  def product3(as: List[Double]): Double =
    foldLeft(as, 1.0)(_ * _)

  def length3[A](as: List[A]): Int =
    foldLeft(as, 0)((b, _) => 1 + b)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((acc, h) => Cons(h, acc))

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def append[A](as: List[A], a: A): List[A] =
    foldRight(as, Cons(a, Nil))(Cons(_, _))

  def concat[A, B](as: List[List[A]]): List[A] =
    foldLeft(as, List[A]())((acc, l) =>
      foldLeft(l, acc)((acc, h) => append(acc, h)))

  def add1(as: List[Int]): List[Int] =
    foldRight(as, Nil: List[Int])((h, acc) => Cons(h + 1, acc))

  def doubleToString(as: List[Double]): List[String] =
    foldRight(as, Nil: List[String])((h, acc) => Cons(h.toString, acc))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((h, acc) => Cons(f(h), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, acc) => if (f(h)) Cons(h, acc) else acc)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)

  def addPairwise(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }

  def zipWith(as: List[Int], bs: List[Int])(f: (Int, Int) => Int): List[Int] =
    (as, bs) match {
      case (Nil, _)                     => Nil
      case (_, Nil)                     => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
}
