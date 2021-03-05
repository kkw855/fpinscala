package com.endsoul.fp.scala
package gettingStarted

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

  // Exercise 2: 배열을 주어진 비교 함수에 의거해서 정렬되어 있는지 점검
  it should "배열을 주어진 비교 함수에 의거해서 정렬되어 있는지 점검한다" in {
    assertResult(true) {
      PolymorphicFunctions.isSorted[Int](Array(1, 3, 5), _ < _)
      PolymorphicFunctions.isSorted[String](Array("A", "D", "F"), _ < _)
    }
  }
}
