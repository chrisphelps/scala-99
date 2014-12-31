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

  test("problem 2: last but one element of a list") {
    assert(Lists.penultimate(List(1)) === None)
    assert(Lists.penultimate(List(1, 1, 2, 3, 5, 8)) === Some(5))
  }

  test("problem 3: kth element of a list") {
    assert(Lists.nth(1, List()) === None)
    assert(Lists.nth(-5, List(1,2,3)) === None)
    assert(Lists.nth(2, List(1, 1, 2, 3, 5, 8)) == Some(2))
  }

  test("problem 4: number of elements in a list") {
    assert(Lists.length(List()) === 0)
    assert(Lists.length(List(1,2,3,4,5)) === 5)
  }
}