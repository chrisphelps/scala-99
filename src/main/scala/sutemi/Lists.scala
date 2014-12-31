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
}
