package com.endsoul.fp.scala
package laziness

import Stream._

class StreamTest extends UnitSpec {
  val stream: Stream[Int] = apply(1, 2, 3, 4, 5)

  "Stream" should "Stream 을 List 로 변환하되 평가를 강제해서 REPL 로 목록의 요소들을 볼 수 있게 하는 함수를 구현한다" in {
    stream.toListRecursive shouldBe List(1, 2, 3, 4, 5)
    stream.toList shouldBe List(1, 2, 3, 4, 5)
    stream.toListFast shouldBe List(1, 2, 3, 4, 5)
  }

  it should "Stream 의 처음 n 개의 요소를 돌려주는 함수 take(n) 과 Stream 처음 n 개의 요소를 건너뛴 스트림을 돌려주는 drop(n) 을 구현한다" in {}
}
