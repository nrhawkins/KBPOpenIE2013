package edu.knowitall.tac2013.prep

import java.util.regex.Pattern
import java.util.LinkedList
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.JavaConverters._

/**
 * Converts KbpRawDocs into KbpParsedDocs,
 * which contain only the KbpDocLines that are relevant.
 */
abstract class KbpDocParser() {
  
  protected val errorCounter = new AtomicInteger(0)
  
  def parseDoc(rawDoc: KbpRawDoc): Option[KbpParsedDoc]
  
  protected def buildDoc(
      docIdLine: Option[KbpDocLine], 
      authorLine: Option[KbpDocLine], 
      dateLine: Option[KbpDocLine], 
      textLines: List[KbpDocLine]): Option[KbpParsedDoc] = {
    
    if (docIdLine.isEmpty) {
      val className = this.getClass().getName()
      val msg = "%s error #%d, no docId found. Sample text follows...".format(className, errorCounter.incrementAndGet())
      System.err.println(msg)
      for (kbpline <- textLines.take(5)) System.err.print(kbpline.line)
      None
    } else {
      Some(new KbpParsedDoc(docIdLine.get, authorLine, dateLine, textLines))
    }
  }
}

class KbpWebDocParser extends KbpDocParser {
  
  override def parseDoc(rawDoc: KbpRawDoc): Option[KbpParsedDoc] = {
    
    var docIdLine = Option.empty[KbpDocLine]
    var authorLine = Option.empty[KbpDocLine]
    var dateLine = Option.empty[KbpDocLine]
    var textLines = new LinkedList[KbpDocLine]
    
    for (kbpLine <- rawDoc.lines; line = kbpLine.line) {
      
      if (docIdLine.isEmpty && line.startsWith("<DOCID>")) docIdLine = Some(kbpLine)
      else if (authorLine.isEmpty && line.startsWith("<POSTER>")) authorLine = Some(kbpLine)
      else if (dateLine.isEmpty && line.startsWith("<DATETIME>")) dateLine = Some(kbpLine)
      else if (!line.startsWith("<")) textLines.add(kbpLine)
    }
    
    buildDoc(docIdLine, authorLine, dateLine, textLines.asScala.toList)    
  }
}
