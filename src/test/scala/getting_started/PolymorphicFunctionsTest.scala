package com.endsoul.fp.scala
package getting_started

class PolymorphicFunctionsTest extends UnitSpec {
  // 배열에서 한 요소를 찾는 다형적 함수
  "다형적 함수" should "배열에서 한 요소를 찾는다" in {
    assertResult(2) {
      PolymorphicFunctions.findFirst[Int](Array(1, 2, 3), _ == 3)
    }
    assertResult(1) {
      PolymorphicFunctions.findFirst[String](Array("AB", "CD", "E"), _ == "CD")
    }
  }

  // Exercise 2.2
  it should "배열을 주어진 비교 함수에 의거해서 정렬되어 있는지 점검한다" in {
    assertResult(true) {
      PolymorphicFunctions.isSorted[Int](Array(1, 3, 5), _ < _)
      PolymorphicFunctions.isSorted[String](Array("A", "D", "F"), _ < _)
    }
  }

  // Exercise 2.3
  it should "인수가 두 개인 함수 f를 인수 하나를 받고 그것으로 f를 부분 적용하는 함수로 변환한다" in {
    assertResult(15) {
      val partial = PolymorphicFunctions.curry((a: Int, b: Int) => a + b)
      val partialA = partial(5)
      partialA(10)
    }
  }

  // Exercise 2.4
  it should "curry 의 변환을 역으로 수행하는 고차 함수 uncurry 를 구현한다" in {
    assertResult(15) {
      val notPartial =
        PolymorphicFunctions.uncurry((a: Int) => (b: Int) => a + b)
      notPartial(5, 10)
    }
  }

  // Exercise 2.5: 두 함수를 합성하는 고차 함수
  it should "두 함수를 합성하는 고차 함수를 구현한다" in {
    assertResult("2020 years!!!") {
      val composed = PolymorphicFunctions.compose((b: String) => b ++ "!!!",
                                                  (a: Int) => s"$a years")
      composed(2020)
    }
  }
}
