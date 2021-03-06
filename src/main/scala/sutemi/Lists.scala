package sutemi

import scala.annotation.tailrec
import scala.util.Random


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

  def insertAt[T](item: T, index: Int, list: List[T]): List[T] = {
    list.splitAt(index) match {
      case (first, rest) => first ::: item :: rest
    }
  }

  def range(from: Int, to: Int): List[Int] = {
    if (from == to)
      List(to)
    else
      from :: range(from + 1, to)
  }

  def randomSelect[T](count: Int, list: List[T]): List[T] = {
    if (count == 0)
      Nil
    else {
      val rnd = new Random().nextInt(list.length)
      val (remainder, item) = removeAt(rnd, list)
      item :: randomSelect(count - 1, remainder)
    }
  }

  def lotto(count: Int, highest: Int): List[Int] = {
    randomSelect(count, range(0, highest))
  }

  def randomPermute[T](list: List[T]): List[T] = {
    randomSelect(list.length, list)
  }

  def combinations[T](count: Int, list: List[T]): List[List[T]] = {
    if (count == 1) list.map{ x => List(x) }
    else {
      for {
        l <- list
        r <- combinations(count - 1, list.filterNot{ t => t == l })
      } yield l :: r
    }
  }

  def subsets[T](list: List[T]): List[(T, List[T])] = list match {
    case Nil => Nil
    case x :: xs => (x, xs) :: subsets(xs)
  }

  // TODO this still isn't right
  def combinationsWithoutReplacement[T](count: Int, list: List[T]): List[List[T]] = {
    if (count == 1) list.map{ x => List(x) }
    else {
      for {
        (l, ls) <- subsets(list)
        r <- combinations(count - 1, ls)
      } yield l :: r
    }
  }

  def group[T](list: List[T]): List[List[List[T]]] = {
    for {
      g1 <- list.combinations(2).toList
      g2 <- list.diff(g1).combinations(3).toList
    } yield List(g1, g2)
  }

  def group[T](groups: List[Int], list: List[T]): List[List[List[T]]] = groups match {
    case Nil => Nil
    case g :: Nil => List(list.combinations(g).toList)
    case g :: gs => for {
      c <- list.combinations(g).toList
      cs <- group(gs, list.diff(c))
    } yield c :: cs
  }

  def lsort[T](list: List[List[T]]): List[List[T]] = list.sortBy(_.length)

  def lsortFreq[T](list: List[List[T]]): List[List[T]] = {
    val freqs = list.groupBy(_.length)
    list.sortBy(x => freqs(x.length).length)
  }
}
