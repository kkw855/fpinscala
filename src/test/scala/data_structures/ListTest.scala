package com.endsoul.fp.scala
package data_structures

import List._

class ListTest extends UnitSpec {
  val intList: List[Int] = List(1, 2, 3, 4, 5)
  val doubleList: List[Double] = List(1.0, 2, 3, 4, 5)

  // Exercise 3.1
  "리스트" should "패턴 매칭 한다" in {
    assertResult(3) {
      intList match {
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
    tail(intList) shouldBe List(2, 3, 4, 5)
  }

  // Exercise 3.3
  it should "첫 요소를 다른 값으로 대체한다" in {
    setHead(intList, 100) shouldBe List(100, 2, 3, 4, 5)
  }

  // Exercise 3.4
  it should "목록에서 처음 n개의 요소를 제거한다" in {
    drop(intList, 3) shouldBe List(4, 5)
  }

  // Exercise 3.5
  it should "주어진 술어와 부합하는 List 의 앞 요소들을 제거한다" in {
    dropWhile(intList)(_ < 4) shouldBe List(4, 5)
  }

  // Exercise 3.6
  it should "마지막 요소룰 제외한 모든 요소로 이루어진 List 를 돌려준다" in {
    init(intList) shouldBe List(1, 2, 3, 4)
  }

  it should "리스트의 모든 요소들을 곱한다" in {
    product2(doubleList) shouldBe 120
  }

  // Exercise 3.8
  it should "z를 Nil 로, f를 Cons 로 바꾸면 리스트를 얻는다" in {
    foldRight(intList, Nil: List[Int])(Cons(_, _)) shouldBe intList
  }

  // Exercise 3.9
  it should "목록의 길이를 계산한다" in {
    List.length(List(1, 2, 3, 4, 5)) shouldBe 5
  }

  // Exercise 3.10
  it should "스택에 안전한 foldLeft 를 구현한다" in {
    foldLeft(intList, Nil: List[Int])((acc, h) => Cons(h, acc)) shouldBe List(5,
                                                                              4,
                                                                              3,
                                                                              2,
                                                                              1)
  }

  // Exercise 3.11
  it should "sum, product, length 계산하는 함수를 foldLeft 를 이용해서 구현한다" in {
    sum3(intList) shouldBe 15
    product3(doubleList) shouldBe 120
    length3(intList) shouldBe 5
  }

  // Exercise 3.12
  it should "목록의 역을 돌려주는 함수를 구현한다" in {
    reverse(intList) shouldBe List(5, 4, 3, 2, 1)
  }

  // Exercise 3.13
  it should "foldRight 를 foldLeft 를 이용해서, foldLeft 를 foldRight 를 이용해서 구현한다" in {
    foldRightViaFoldLeft(intList, Nil: List[Int])(Cons(_, _)) shouldBe intList
    // TODO: 나머지도 구현
  }

  // Exercise 3.14
  it should "foldRight 를 이용해서 append 를 구현한다" in {
    append(intList, 6) shouldBe List(1, 2, 3, 4, 5, 6)
  }

  // Exercise 3.15
  it should "목록들의 목록을 하나의 목록으로 연결한다" in {
    val list = List(List(1, 2), List(3, 4), List(5))
    concat(list) shouldBe List(1, 2, 3, 4, 5)
  }

  // Exercise 3.16
  it should "정수 목록의 각 요소에 1을 더해서 목록을 변환한다" in {
    add1(intList) shouldBe List(2, 3, 4, 5, 6)
  }

  // Exercise 3.17
  it should "List[Double] 의 각 값을 String 으로 변환한다" in {
    doubleToString(doubleList) shouldBe List("1.0", "2.0", "3.0", "4.0", "5.0")
  }

  // Exercise 3.18
  it should "목록의 구조를 유지하면서 목록의 각 요소를 수정하는 작업을 일반화한 함수 map 을 구현한다" in {
    map(intList)(_ * 10) shouldBe List(10, 20, 30, 40, 50)
  }

  // Exercise 3.19
  it should "목록에서 주어진 술어를 만족하지 않는 요소들을 제거하는 함수 filter 를 구현한다" in {
    filter(intList)(_ > 3) shouldBe List(4, 5)
  }

  // Exercise 3.20
  it should "map 과 비슷하되 하나의 요소가 아니라 목록을 최종 결과 목록에 삽입하는 함수 flatMap 을 구현한다" in {
    flatMap(intList)(a => List(a, a)) shouldBe List(1, 1, 2, 2, 3, 3, 4, 4, 5,
      5)
  }

  // Exercise 3.21
  it should "flatMap 을 이용해서 filter 를 구현한다" in {
    filter2(intList)(_ > 3) shouldBe List(4, 5)
  }

  // Exercise 3.22
  it should "목록 두 개를 받아서 대응되는 요소들을 더한 값들로 이루어진 새 목록을 구축하는 함수를 구현한다" in {
    addPairwise(List(1, 2, 3), List(4, 5, 6)) shouldBe List(5, 7, 9)
    addPairwise(List(1, 2), List(4, 5, 6)) shouldBe List(5, 7)
  }

  // Exercise 3.23
  it should "3.22의 함수를 정수나 덧셈에 국한되지 않도록 일반화하는 함수 zipWith 를 구현한다" in {
    zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) shouldBe List(5, 7, 9)
  }

  // Exercise 3.24
  it should "List 가 또 다른 List 를 부분 순차열로서 담고 있는지 점검하는 hasSubsequence 함수를 구현한다" in {
    // TODO: 구헌
    // 리스트 안의 숫자는 정렬되어 있다고 가정
  }
}
