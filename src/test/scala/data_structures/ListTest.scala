package com.endsoul.fp.scala
package data_structures

import data_structures.List._

import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ListTest extends UnitSpec {
  val list: List[Int] = List(1, 2, 3, 4, 5)

  // Exercise 3.1
  "리스트" should "패턴 매칭 한다" in {
    assertResult(3) {
      list match {
        case Cons(x, Cons(2, Cons(4, _)))          => x
        case Nil                                   => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t)                            => h + sum(t)
        case _                                     => 101
      }
    }
  }

  // Exercise 3.2
  it should "첫 요소를 제거한다" in {
    tail(list) shouldBe List(2, 3, 4, 5)
  }

  // Exercise 3.3
  it should "첫 요소를 다른 값으로 대체한다" in {
    setHead(list, 100) shouldBe List(100, 2, 3, 4, 5)
  }

  // Exercise 3.4
  it should "목록에서 처음 n개의 요소를 제거한다" in {
    drop(list, 3) shouldBe List(4, 5)
  }

  // Exercise 3.5
  it should "주어진 술어와 부합하는 List 의 앞 요소들을 제거한다" in {
    dropWhile(list)(_ < 4) shouldBe List(4, 5)
  }

  // Exercise 3.6
  it should "마지막 요소룰 제외한 모든 요소로 이루어진 List 를 돌려준다" in {
    init(list) shouldBe List(1, 2, 3, 4)
  }

  it should "리스트의 모든 요소들을 곱한다" in {
    product2(List(1.0, 2.0, 3.0, 4.0, 5.0)) shouldBe 120
  }

  // Exercise 3.8
  it should "z를 Nil 로, f를 Cons 로 바꾸면 리스트를 얻는다" in {
    foldRight(list, Nil: List[Int])(Cons(_, _)) shouldBe list
  }

  // Exercise 3.9
//  it should "목록의 길이를 계산한다" in {
//    length(List(1, 2, 3, 4, 5)) shouldBe 5
//  }
}