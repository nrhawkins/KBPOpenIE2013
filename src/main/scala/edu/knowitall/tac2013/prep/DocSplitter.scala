package edu.knowitall.tac2013.prep

import java.io.File
import edu.knowitall.common.Resource
import java.util.LinkedList
import java.util.regex.Pattern
import scala.collection.JavaConverters._

/**
 * Reads KBP corpus and provides an iterator over
 * document elements (<DOC>) in the corpus.
 */
class DocSplitter {
  
  private val docCloseTag = Pattern.compile("\\s*(</DOC>|</doc>)\\s*")
  
  def splitDocs(file: File): Iterator[KbpDoc] = {
    
    Resource.using(io.Source.fromFile(file)) { source =>
      splitDocs(source)
    }
  }
  
  def splitDocs(source: io.Source): Iterator[KbpDoc] = {
    splitDocs(source.getLines)
  }
  
  def splitDocs(lines: Iterator[String])= new Iterator[KbpDoc] {
    
    def hasNext = lines.hasNext
    
    def next = getNextDoc(lines)
  }
  
  def getNextDoc(lines: Iterator[String]): KbpDoc = {
    
    val lineBuffer = new LinkedList[String]()
    
    var done = false
    
    while (!done && lines.hasNext) {
      
      val nextLine = lines.next()
      if (docCloseTag.matcher(nextLine).matches()) done = true
      lineBuffer.add(nextLine)
    }
    new KbpDoc(lineBuffer.asScala.toList)
    
  }
}

class KbpDoc(val lines: List[String])