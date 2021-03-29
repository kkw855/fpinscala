package com.endsoul.fp.scala
package data_structures

import Tree._

class TreeTest extends UnitSpec {
  val tree: Tree[Int] =
    Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

  // Exercise 3.25
  "트리" should "트리의 노드, 즉 잎(leaf)과 가지(branch)의 개수를 세는 함수 size 를 구현한다" in {
    Tree.size(tree) shouldBe 7
  }

  // Exercise 3.26
  it should "가장 큰 요소를 돌려주는 함수 maximum 을 구현한다" in {
    maximum(tree) shouldBe 4
  }

  // Exercise 3.27
  it should "트리의 뿌리(root)에서 임의의 잎으로의 가장 긴 경로의 길이를 돌려주는 함수 depth 를 구현한다" in {
    depth(tree) shouldBe 3
  }

  // Exercise 3.28
  it should "List 에 대한 함수 map 과 비슷하게 트리의 각 요소를 주어진 함수로 수정하는 함수 map 을 구현한다" in {
    map(tree)(_ * 10) shouldBe Branch(Branch(Leaf(10), Leaf(20)),
                                      Branch(Leaf(30), Leaf(40)))
  }

  // Exercise 3.29
  it should "size 와 maximum, depth, map 의 유사성을 요약해서 일반화한 새 함수 fold 를 구현하고 " +
    "그 함수들을 새 fold 를 이용해서 다시 구현한다" in {
    size2(tree) shouldBe 7
    maximum2(tree) shouldBe 4
    depth2(tree) shouldBe 3
    map2(tree)(_ * 10) shouldBe Branch(Branch(Leaf(10), Leaf(20)),
                                       Branch(Leaf(30), Leaf(40)))
  }
}
