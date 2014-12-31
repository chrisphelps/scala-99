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
}
