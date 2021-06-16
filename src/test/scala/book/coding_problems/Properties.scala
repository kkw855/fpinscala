package com.endsoul.fp.scala
package book.coding_problems

import org.scalacheck.Gen.const
import org.scalacheck.Prop.{AnyOperators, all, atLeastOne, propBoolean, within}
import org.scalacheck.{Gen, Prop, Test}

object Properties extends App {
  import org.scalacheck.Prop.forAll

  val stringLengthProp: Prop = forAll { (_: String).length >= 0 }
  stringLengthProp.check()

  /* Commutativity: 피연산자의 순서가 바껴도 결과는 동일하다 */

  // + 와 * 연산
  forAll((a: Int, b: Int) => a + b == b + a).check()
  forAll((a: Int, b: Int) => a * b == b * a).check()

  // 문자열 + 연산은 Commutativity 가 아니다
  forAll((a: String, b: String) => a + b == b + a).check()
  // 한 쪽이 빈 문자열 일 때는 Commutativity
  forAll((a: String) => a + "" == a + "").check()

  /* Associativity: 피연사자의 순서가 변경되지 않는다면 어느 부분을 먼저 연산하던 결과는 동일하다. */

  // + 와 * 연산
  forAll((a: Int, b: Int, c: Int) => (a + b) + c == a + (b + c)).check()
  forAll((a: Int, b: Int, c: Int) => (a * b) * c == a * (b * c)).check()

  // 문자열 + 연산은 Associativity 이다
  forAll((a: String, b: String, c: String) => (a + b) + c == a + (b + c))
    .check()

  /* Identity: 어떤 연산에 특정 피연산자 값을 넣으면 변경되지 않은 본래의 값이 나온다 */
  forAll((a: Int) => a + 0 == a && 0 + a == a).check()
  forAll((a: Int) => a * 1 == a && 1 * a == a).check()

  // 문자열의 + 연산의 Commutativity 는 Identity 값이 적용될 때만 가능하다
  forAll((a: String) => a + "" == a && "" + a == a).check()

  /* Invariants: 어떤 연산을 적용해도 변경되지 않는 것 */

  // 문자열 sort, toUpperCase 연산을 수행해도 length 는 변하지 않는다
  forAll(Gen.asciiStr)((a: String) => a.toUpperCase.length == a.length).check()
  forAll(Gen.asciiStr)((a: String) => a.sorted.length == a.length).check()

  /* Idempotence: 연산이 오직 한 번만 피연산자를 변경한다 */

  // 첫 번째 sort, toUpperCase 연산만 피연산자를 변경하고, 그 이후에 연산은 아무것도 변경하지 않는다
  forAll((a: String) => a.toUpperCase.toUpperCase == a.toUpperCase).check()
  forAll((a: String) => a.sorted.sorted == a.sorted).check()

  forAll((a: Int) => a * 0 * 0 == a * 0).check()

  /* Symmetry(round-trip): 본래의 값으로 되돌리는 어떤 pair */

  forAll((a: String) => a.reverse.reverse == a)
  forAll((a: Int, b: Int) => a + b - b == a)

  /* Checking property */

  val prop = forAll { a: String =>
    a.nonEmpty ==> (a.reverse.reverse == a)
  }

  val timed = within(10000)(prop)

//  Test.check(timed) {
//    _.withMinSuccessfulTests(100000).withWorkers(4).withMaxDiscardRatio(3)
//  }

  /* Combining Properties */

  forAll { (a: Int, b: Int, c: Int, d: String) =>
    val multiplicationLaws = all(
      "Commutativity" |: (a * b ?= b * a),
      "Associativity" |: ((a * b) * c ?= a * (b * c)),
      "Identity" |: all(a * 1 == a, 1 * a == a)
    ) :| "Multiplication laws"
    val stringProps = atLeastOne(d.isEmpty, d.nonEmpty)
    all(multiplicationLaws, stringProps)
  }.check()
}
