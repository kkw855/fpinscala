package com.endsoul.fp.scala
package error_handling

class EitherTest extends UnitSpec {
  "Either" should "Right 값에 대해 작용하는 버전의 map, flatMap, orElse, map2, Either 함수를 구현한다" in {
    Right(1).map(_ * 10) shouldBe Right(10)
    Left("Error").map((a: Int) => a * 10) shouldBe Left("Error")

    Right(1).flatMap(a => Right(a * 10)) shouldBe Right(10)

    Left("Error").orElse(Right(10)) shouldBe Right(10)
  }
}
