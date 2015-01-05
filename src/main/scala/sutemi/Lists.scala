package sutemi

import scala.annotation.tailrec


object Lists {

  def hello = "Hello"

  @tailrec
  def last[T](list: List[T]): Option[T] = list match {
    case Nil => None
    case x::Nil => Some(x)
    case _::xs => last(xs)
  }

  @tailrec
  def penultimate[T](list: List[T]): Option[T] = list match {
    case Nil => None
    case x::Nil => None
    case x::_::Nil => Some(x)
    case _::xs => penultimate(xs)
  }

  // this doesn't really handle negative indices directly, but will work for most cases
  @tailrec
  def nth[T](pos: Int, list: List[T]): Option[T] = (pos, list) match {
    case (_, Nil) => None
    case (0, x::_) => Some(x)
    case (k, _::xs) => nth(k - 1, xs)
  }

  def length[T](list: List[T]): Int = {
    def count(acc: Int, list: List[T]): Int = list match {
      case Nil => acc
      case x::xs => count(acc + 1, xs)
    }
    count(0, list)
  }

  def reverse[T](list: List[T]): List[T] = {
    def traverse(acc: List[T], list: List[T]): List[T] = list match {
      case Nil => acc
      case x::xs => traverse(x :: acc, xs)
    }
    traverse(Nil, list)
  }

  def reverseFold[T](list: List[T]): List[T] = {
    list.foldLeft(List[T]())((ls, x) => x :: ls)
  }

  def isPalindrome[T](list: List[T]): Boolean = list match {
    case Nil => false
    case _ => reverse(list) == list
  }

  def flatten(list: List[Any]): List[Any] = {
    def consAll(head: List[Any], rest: List[Any]): List[Any] = head match {
      case Nil => flatten(rest)
      case f :: fs => f :: consAll(fs, rest)
    }

    list match {
      case Nil => Nil
      case (x:List[Any]) :: xs => consAll(x, flatten(xs))
      case x :: xs => x :: flatten(xs)
    }
  }

  def compress[T](list: List[T]): List[T] = {
    def compressAcc(acc: T, list: List[T]): List[T] = list match {
      case Nil => Nil
      case x :: xs if x == acc => compressAcc(acc, xs)
      case x :: xs => x :: compressAcc(x, xs)
    }

    list match {
      case Nil => Nil
      case x :: xs => x :: compressAcc(x, xs)
    }
  }

  def pack[T](list: List[T]): List[List[T]] = list match {
    case Nil => Nil
    case x :: xs => pack(xs) match {
      case Nil => List(x) :: Nil
      case head :: rest if head.head == x => (x :: head) :: rest
      case head :: rest => List(x) :: head :: rest
    }
  }

  def encode[T](list: List[T]): List[(Int, T)] = {
    pack(list).map(l => (l.size, l.head))
  }

  def encodeModified[T](list: List[T]): List[Any] = {
    pack(list).map(l => if (l.size > 1) (l.size, l.head) else l.head)
  }

  def decode[T](list: List[(Int, T)]): List[T] = {
    def repeat(n: Int, item: T): List[T] =
      if (n == 1) List(item)
      else item :: repeat(n-1, item)
    list.flatMap {case (times, item) => repeat(times, item)}
  }

  def encodeDirect[T](list: List[T]): List[(Int, T)] = list match {
    case Nil => Nil
    case x :: xs => {
      val (same, rest) = list.span(_ == x)
      (same.length, x) :: encodeDirect(rest)
    }
  }

  def duplicate[T](list: List[T]): List[T] = duplicateN(2, list)

  def duplicateN[T](times: Int, list: List[T]): List[T] = list flatMap { List.fill(times)(_) }

  def drop[T](ordinal: Int, list: List[T]): List[T] = {
    def loop(current: Int, ordinal: Int, list: List[T]): List[T] = list match {
      case Nil => Nil
      case l :: ls =>
        if (current == ordinal) loop(1, ordinal, ls)
        else l :: loop(current + 1, ordinal, ls)
    }
    loop(1, ordinal, list)
  }

  def split[T](count: Int, list: List[T]): (List[T], List[T]) = {
    val first = list.take(count)
    val rest = list.drop(count)
    (first, rest)
  }

  def slice[T](fromIdx: Int, toIdx: Int, list: List[T]): List[T] = {
    val (first, rest) = split(fromIdx, list)
    val (slice, tail) = split(toIdx - fromIdx, rest)
    slice
  }

  def rotate[T](count: Int, list: List[T]): List[T] = {
    def rotateIt(count: Int, list: List[T]) = {
      val (first, rest) = split(count, list)
      rest ++ first
    }
    if (count == 0) list
    else if (count < 0) rotateIt(count + list.length, list)
    else rotateIt(count, list)
  }

  def removeAt[T](index: Int, list: List[T]): (List[T], T) = list match {
      // if list is Nil, it means that the index was greater than the original list length
      // TODO maybe this should return more like (List[T], Option[T])?
    case l :: ls =>
      if (index == 0) (ls, l)
      else {
        val (rest, item) = removeAt(index - 1, ls)
        (l :: rest, item)
      }
  }

}
