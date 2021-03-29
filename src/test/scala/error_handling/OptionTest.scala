package com.endsoul.fp.scala
package error_handling

import Option._

class OptionTest extends UnitSpec {
  // Exercise 4.1
  "Option" should "Option 에 대한 map, flatMap, getOrElse, orElse, filter 함수들을 구현한다" in {
    Some(5).map(_ * 10) shouldBe Some(50)
    Some(5).flatMap(a => Some(a * 10)) shouldBe Some(50)
    None.getOrElse(50) shouldBe 50
    None.orElse(Some(50)) shouldBe Some(50)

    Some(5).filter(_ > 10) shouldBe None
    Some(5).filter(_ < 10) shouldBe Some(5)
  }

  // Exercise 4.3
  it should "두 Option 값을 이항 함수(binary function)를 이용해서 결합하는 일반적 함수 map2 를 구현한다" in {
    map2(Some(2), Some(3))(_ * _) shouldBe Some(6)
    map2(Some(2), None: Option[Int])(_ * _) shouldBe None
    map2(None: Option[Int], Some(3))(_ * _) shouldBe None
  }

  // Exercise 4.4
  it should "Option 들의 목록을 받고, 그 목록에 있는 모든 Some 값으로 구성된 목록을 담은 Option 을 돌려주는 함수 sequence 를 구현한다" in {
    sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
    sequence(List(Some(1), None, Some(3))) shouldBe None
  }

  // Exercise 4.5
  it should "traverse 함수를 구현하고, traverse 를 사용해서 sequence 함수를 구현한다" in {
    // TODO: 추가
  }
}
