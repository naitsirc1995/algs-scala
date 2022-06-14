package com.rockthejvm.lists

import scala.annotation.tailrec

sealed abstract class RList[+T] {
  /*
  * Standard functions*/
  def head:T
  def tail:RList[T]
  def isEmpty:Boolean

  def ::[S>:T](elem:S):RList[S] = new ::(elem,this)

  def apply(index:Int):T

  def length:Int
}

case object RNill extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException

  override def length: Int = 0
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
}

object ListProblems extends App {
  val aSmallList = 1 :: 2 :: 3 :: RNill // Rnill.::(3).::(2)::.::(1)

  // test get-kth
  println(aSmallList.apply(0))
  println(aSmallList.apply(1))
  println(aSmallList.apply(2))
  //println(aSmallList.apply(90))

  println("\n")
  // test length
  println("Now a length example")
  println(aSmallList.length)
}
