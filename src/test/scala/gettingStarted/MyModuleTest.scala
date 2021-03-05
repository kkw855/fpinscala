package com.endsoul.fp.scala
package gettingStarted

class MyModuleTest extends UnitSpec {
  //  Exercise 1: 꼬리 재귀 함수를 사용해서 피보나치 수열 계산
  "고차함수" should "피보나치 수열을 계산한다" in {
    assert(MyModule.fib(0) == 0)
    assert(MyModule.fib(5) == 5)
    assert(MyModule.fib(6) == 8)
  }
}
