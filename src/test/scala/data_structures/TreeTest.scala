package com.endsoul.fp.scala
package data_structures

import Tree._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class TreeTest extends UnitSpec {
  val tree: Tree[Int] =
    Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

  // Exercise 3.25
  "트리" should "트리의 노드, 즉 잎(leaf)과 가지(branch)의 개수를 세는 함수 size 를 구현한다" in {
    size(tree) shouldBe 7
  }

  it should "가장 큰 요소를 돌려주는 함수 maximum 을 구현한다" in {
    maximum(tree) shouldBe 4
  }
}
