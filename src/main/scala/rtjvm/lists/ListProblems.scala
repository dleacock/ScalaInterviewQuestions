package rtjvm.lists


import scala.annotation.tailrec

sealed abstract class RList[+T] {
  def head: T

  def tail: RList[T]

  def isEmpty: Boolean

  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  def apply(index: Int): T

  def length: Int

  def reverse: RList[T]

  def ++[S >: T](anotherList: RList[S]): RList[S]

  def removeAt(index: Int): RList[T]

  def map[S](f: T => S): RList[S]

  def flatMap[S](f: T => RList[S]): RList[S]

  def filter(f: T => Boolean): RList[T]
}

// Since RList is covariance => a list[nothing] is a good substitute for a list[ints]
case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException

  override def tail: RList[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NotImplementedError()

  override def length: Int = 0

  override def reverse: RList[Nothing] = this

  override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

  override def removeAt(index: Int): RList[Nothing] = this

  override def map[S](f: Nothing => S): RList[S] = this

  override def flatMap[S](f: Nothing => RList[S]): RList[S] = this

  override def filter(f: Nothing => Boolean): RList[Nothing] = this
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false

  override def apply(index: Int): T = {
    @tailrec
    def recursiveSearch(remaining: RList[T], currentDepth: Int): T = {
      if (remaining.isEmpty) throw new NoSuchElementException
      else if (currentDepth equals index) remaining.head
      else recursiveSearch(remaining.tail, currentDepth + 1)
    }

    recursiveSearch(this, 0)
  }

  override def length: Int = {
    @tailrec
    def recursiveLengthFind(remaining: RList[T], currentLength: Int): Int = {
      if (remaining.isEmpty) currentLength
      else recursiveLengthFind(remaining.tail, currentLength + 1)
    }

    recursiveLengthFind(this, 0)
  }

  override def reverse: RList[T] = {
    @tailrec
    def recursiveReverse(remaining: RList[T], result: RList[T]): RList[T] = {
      if (remaining.isEmpty) result
      else recursiveReverse(remaining.tail, remaining.head :: result)
    }

    recursiveReverse(this, RNil)
  }

  override def ++[S >: T](anotherList: RList[S]): RList[S] = {
    @tailrec
    def recursiveConcat(remaining: RList[S], current: RList[S]): RList[S] = {
      if (remaining.isEmpty) current
      else recursiveConcat(remaining.tail, remaining.head :: current)
    }

    recursiveConcat(anotherList, this.reverse).reverse
  }

  override def removeAt(index: Int): RList[T] = {

    @tailrec
    def recursiveRemoveAt(remaining: RList[T], current: RList[T], currentIndex: Int): RList[T] = {
      if (remaining.isEmpty && currentIndex > remaining.length) current
      else if (currentIndex == index) recursiveRemoveAt(remaining.tail, current, currentIndex + 1)
      else recursiveRemoveAt(remaining.tail, remaining.head :: current, currentIndex + 1)
    }

    recursiveRemoveAt(this, RNil, 0).reverse
  }

  override def map[S](f: T => S): RList[S] = {
    @tailrec
    def recursiveMap(remaining: RList[T], current: RList[S]): RList[S] = {
      if (remaining.isEmpty) current
      else recursiveMap(remaining.tail, f(remaining.head) :: current)
    }

    recursiveMap(this, RNil).reverse
  }

  override def flatMap[S](f: T => RList[S]): RList[S] = {
    @tailrec
    def recursiveFlatMap(remaining: RList[T], current: RList[S]): RList[S] = {
      if (remaining.isEmpty) current.reverse
      else recursiveFlatMap(remaining.tail, f(remaining.head).reverse ++ current)
    }

    recursiveFlatMap(this, RNil)
  }

  override def filter(f: T => Boolean): RList[T] = {
    @tailrec
    def recursiveFilter(remaining: RList[T], current: RList[T]): RList[T] = {
      if (remaining.isEmpty) current
      else if (f(remaining.head)) recursiveFilter(remaining.tail, remaining.head :: current)
      else recursiveFilter(remaining.tail, current)
    }

    recursiveFilter(this, RNil).reverse
  }

  override def toString: String = {
    @tailrec
    def recursiveToString(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else recursiveToString(remaining.tail, s"$result${remaining.head},")
    }

    s"[${recursiveToString(this, "")}]"
  }
}

object RList {
  def from[T](iterable: Iterable[T]): RList[T] = {
    @tailrec
    def convertToRList(remaining: Iterable[T], acc: RList[T]): RList[T] = {
      if (remaining.isEmpty) acc
      else convertToRList(remaining.tail, remaining.head :: acc)
    }

    convertToRList(iterable, RNil).reverse
  }
}


object ListProblems extends App {
  //RNil.::(2) == 2 :: RN
  private val smallList: RList[Int] = 1 :: 2 :: 3 :: 4 :: 5 :: RNil
  private val list1: RList[Int] = 1 :: 2 :: 3 :: RNil
  private val list2: RList[Int] = 4 :: 5 :: 6 :: RNil
  println(smallList)

  println(smallList(4))
  println(smallList.length)
  println(smallList.reverse)

  private val value: RList[Int] = list1 ++ list2
  println(value)

  println(smallList.removeAt(3))

  println(smallList.filter(x => x % 2 == 0))

  println(smallList.map(x => x + 1))

  println(smallList.map(x => List(x, x + 1)))
  println(smallList.flatMap(x => x :: x + 1 :: RNil))
}
