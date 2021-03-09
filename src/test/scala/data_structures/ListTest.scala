package com.endsoul.fp.scala
package data_structures

import data_structures.List.{sum, tail}

import org.scalatest.matchers.must.Matchers.contain
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.language.postfixOps

class ListTest extends UnitSpec {
  // Exercise 3.1
  "리스트" should "패턴 매칭 한다" in {
    assertResult(3) {
      List(1, 2, 3, 4, 5) match {
        case Cons(x, Cons(2, Cons(4, _)))          => x
        case Nil                                   => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t)                            => h + sum(t)
        case _                                     => 101
      }
    }
  }

  // Exercise 3.2
  it should "리스트의 첫 요소를 제거한다" in {
    assertResult(true) {
      tail(List(1, 2, 3, 4, 5)) == List(2, 3, 4, 5)
    }
  }

  // Exercise 3.3
  it should "리스트의 첫 요소를 다른 값으로 대체한다" in {
    assertResult(true) {
      setHead(List(1, 2, 3, 4, 5)) == List(2, 3, 4, 5)
    }
  }
}
