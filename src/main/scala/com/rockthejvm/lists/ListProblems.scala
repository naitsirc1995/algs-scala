package com.rockthejvm.lists

import scala.annotation.tailrec
import scala.collection.immutable.Stream.Empty

sealed abstract class RList[+T] {
  /*
  * Standard functions*/
  def head:T
  def tail:RList[T]
  def isEmpty:Boolean

  def ::[S>:T](elem:S):RList[S] = new ::(elem,this)

  def apply(index:Int):T

  def length:Int

  def reverse:RList[T]

  // concatenate another list to this one

  def ++[S>:T](anotherList:RList[S]):RList[S]

  // remove an element at a given index, return a NEW list
  def removeAt(index:Int):RList[T]
}

case object RNill extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException

  override def length: Int = 0

  override def reverse: RList[Nothing] = RNill

  override def ++[S>:Nothing](anotherList:RList[S]):RList[S] = anotherList

  override def removeAt(index: Int): RList[Nothing] = RNill


}

case class ::[+T](override val head:T, override val tail:RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false

  override def toString: String = {
    @tailrec
    def toStringTailRec(remaining:RList[T],result:String):String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailRec(remaining.tail,s"$result${remaining.head}, ")
    }
    "["+toStringTailRec(this,"")+"]"
  }

  override def apply(index: Int): T = {
    /*
      [1,2,3,4,5].apply(2) = applyTailRec([1,2,3,4,5],0)
      = applyTailRec([2,3,4,5],1) = applyTailRec([3,4,5],2)
      = 3

      Complexity of this algorithm?
      O(min(N,index))
    * */
    @tailrec
    def applyTailRec(remaining:RList[T],currentIndex:Int):T = {
      if (currentIndex == index) remaining.head
      else applyTailRec(remaining.tail,currentIndex+1)
    }

    if (index < 0 ) throw new NoSuchElementException
    else applyTailRec(this,0)
  }

  override def length: Int = {
    /*
    [1,2,3,4,5].length = lengthTailRec([1,2,3,4,5],0)
    = lengthTailRec([2,3,4,5],1)
    = lengthTailRec([3,4,5],2)
    = lengthTailRec([4,5],3)
    = lengthTailRec([5],4)
    = lengthTailRec([],5)
    = 5

    Complexity: O(N)
    * */
    @tailrec
    def lengthTailRec(remainingList:RList[T],accumulator:Int):Int = {
      if (remainingList.isEmpty) accumulator
      else lengthTailRec(remainingList.tail,accumulator+1)
    }

    lengthTailRec(this,0)
  }

  // reverse this list into a new list
  override def reverse: RList[T] = {
    @tailrec
    def reverseTailRec(remainingList:RList[T],result:RList[T]):RList[T] = {
      if (remainingList.isEmpty) result
      else reverseTailRec(remainingList.tail, remainingList.head :: result)
    }
    reverseTailRec(this,RNill)
  }

  override def ++[S>:T](anotherList:RList[S]):RList[S] = {

    def concatTailrec(remainingList:RList[S], acc:RList[S]):RList[S] = {
      if (remainingList.isEmpty) acc
      else (concatTailrec(remainingList.tail, remainingList.head :: acc))
    }

    concatTailrec(anotherList,this.reverse).reverse
  }

  override def removeAt(index:Int):RList[T] = {

    /*
      [1,2,3,4,5].removeAt(2) = removeAtTailrec([1,2,3,4,5],0,[])
      = removeAtTailrec([2,3,4,5],1,[1])
      = removeAtTailrec([3,4,5],2,[1,2])
      = [1,2] ++ [3,4,5]
    * */
    @tailrec
    def removeAtTailrec(remaining:RList[T],currentIndex:Int,predecesors:RList[T]):RList[T] = {
      if (currentIndex == index) predecesors.reverse ++ remaining.tail
      else if (remaining.isEmpty) predecesors.reverse
      else removeAtTailrec(remaining.tail,currentIndex+1,remaining.head :: predecesors)
    }

    if (index < 0 ) this
    else removeAtTailrec(this,0,RNill)
  }
}

object RList {
  def from[T](iterable:Iterable[T]):RList[T] = {
    def convertToRListTailrec(remaining:Iterable[T],acc:RList[T]):RList[T] = {
      if (remaining.isEmpty) acc
      else convertToRListTailrec(remaining.tail, remaining.head::acc)
    }

    convertToRListTailrec(iterable,RNill).reverse
  }
}

object ListProblems extends App {
  val aSmallList = 1 :: 2 :: 3 :: RNill // Rnill.::(3).::(2)::.::(1)
  val aLargeList = RList.from(1 to 10000)

  val concat1 = RList.from(1 to 3)
  val concat2 = RList.from(4 to 5)

  // test get-kth
  println(aSmallList.apply(0))
  println(aSmallList.apply(1))
  println(aSmallList.apply(2))
  println(aLargeList.apply(8735))
  //println(aSmallList.apply(90))

  println("\n")
  // test length
  println("Now a length example")
  println(aSmallList.length)
  println(aLargeList.length)

  println("\n")
  println("Now testing revers")
  println(aSmallList.reverse)
  println(aLargeList.reverse)

  println("\n")
  println("Now testing the concat method")
  println(aSmallList ++ aLargeList)
  println(concat1 ++ concat2)

  println("\n")
  println("Now the removal part")
  println(concat1.removeAt(2))
  println(aLargeList.removeAt(13))


}
