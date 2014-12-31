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

  test("problem 5: reverse a list") {
    assert(Lists.reverse(List()) === List())
    assert(Lists.reverse(List(1, 1, 2, 3, 5, 8)) === List(8, 5, 3, 2, 1, 1))
  }

  test("problem 5: reverse a list with fold") {
    assert(Lists.reverseFold(List()) === List())
    assert(Lists.reverseFold(List(1, 1, 2, 3, 5, 8)) === List(8, 5, 3, 2, 1, 1))
  }

  test("problem 6: palindrome") {
    assert(Lists.isPalindrome(List()) === false)
    assert(Lists.isPalindrome(List(1, 2, 3, 2, 1)) === true)
    assert(Lists.isPalindrome(List(1, 2, 3, 4, 5)) === false)
  }
}