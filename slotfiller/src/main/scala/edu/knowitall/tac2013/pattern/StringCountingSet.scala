package edu.knowitall.tac2013.pattern

import scala.collection.mutable

class StringCounter private (private var backingMap: mutable.Map[String, MutInt] = new mutable.HashMap[String, MutInt]) {
  
  def add(s: String): StringCounter = add(s, 1)
  
  private def add(s: String, n: Int): StringCounter = {
    backingMap.getOrElseUpdate(s, new MutInt).add(n)
    this
  }
  
  /**
   * Keep the top strings only
   */
  def trim(maxStrings: Int): StringCounter = {
    val newMap = backingMap.iterator.toSeq.sortBy(-_._2.value).take(maxStrings).toMap
    backingMap.clear()
    backingMap ++= newMap
    this
  }
  
  def map = backingMap.toMap
  
  def top(num: Int) = {
    backingMap.iterator.toSeq.sortBy(-_._2.value).take(num).map { case (str, count) => (str, count.value) }
  }
  
  def addAll(other: StringCounter): StringCounter = {
    def allElements = (backingMap.iterator ++ other.backingMap.iterator).toSeq
    val newMap = allElements.groupBy(_._1).map { case (str, strCounts) => 
      val newCount = new MutInt(strCounts.map(_._2.value).sum)
      (str, newCount)
    }
    backingMap.clear()
    backingMap ++= newMap
    this
  }
}

class MutInt(var value: Int = 0) {
  def inc: Unit = add(1)
  def add(n: Int) = value += n
}

object StringCounter {
  
  def empty = fromStrings(Seq.empty)
  
  def fromStrings(strings: Seq[String]) = {
    val counter = new StringCounter() 
    strings.foreach(counter.add)
    counter
  }
}