package com.endsoul.fp.scala
package error_handling

import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

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
}
