package com.endsoul.fp.scala
package getting_started

class MyModuleTest extends UnitSpec {
  //  Exercise 2.1
  "고차함수" should "피보나치 수열을 계산한다" in {
    assert(MyModule.fib(0) == 0)
    assert(MyModule.fib(5) == 5)
    assert(MyModule.fib(6) == 8)
  }
}
