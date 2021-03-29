package com.endsoul.fp.scala
package laziness

import Stream._

class StreamTest extends UnitSpec {
  val stream: Stream[Int] = apply(1, 2, 3, 4, 5)

  "Stream" should "Stream 을 List 로 변환하되 평가를 강제해서 REPL 로 목록의 요소들을 볼 수 있게 하는 함수를 구현한다" in {
    stream.toList shouldBe List(1, 2, 3, 4, 5)
  }
}
