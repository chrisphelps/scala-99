package sutemi

import org.scalatest.FunSuite

class ListSuite extends FunSuite {

  test("wiring test") {
    assert(Lists.hello === "Hello")
  }

  test("problem 1: last element of a list") {
    assert(Lists.last(List()) === None)
    assert(Lists.last(List(1,2,3,4,5)) === Some(5))
  }
}