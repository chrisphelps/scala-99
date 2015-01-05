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

  test("problem 7: flatten") {
    assert(Lists.flatten(List(List(1, 1), List (1, 2))) === List(1, 1, 1, 2))
    assert(Lists.flatten(List(List(1, 1), 2, List(3))) === List(1, 1, 2, 3))
    assert(Lists.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) === List(1, 1, 2, 3, 5, 8))
  }

  test("problem 8: remove consecutive dupes") {
    assert(Lists.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List('a, 'b, 'c, 'a, 'd, 'e))
  }

  test("problem 9: pack consecutive dupes") {
    assert(Lists.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  test("problem 10: run-length encoding") {
    assert(Lists.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  test("problem 11: run-length encoding modified") {
    assert(Lists.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
  }

  test("problem 12: decode run-length encoding") {
    assert(Lists.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) === List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  test("problem 13: run-length encoding directly") {
    assert(Lists.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  test("problem 14: duplicate elements of a list") {
    assert(Lists.duplicate(List('a, 'b, 'c, 'c, 'd)) === List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  test("problem 15: repeat elements of a list") {
    assert(Lists.duplicateN(3, List('a, 'b, 'c, 'c, 'd)) === List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  test("problem 16: drop elements from a list") {
    assert(Lists.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) === List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  test("problem 17: split a list") {
    assert(Lists.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) === (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }
}