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
}
