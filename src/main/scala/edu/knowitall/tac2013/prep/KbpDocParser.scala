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
  
  protected def isValidText(line: String): Boolean = {
    !line.startsWith("<")
  }
}

object KbpDocParser {
  
  def getParser(corpus: String) = corpus match {
    case "web" => new KbpWebDocParser()
    case "news" => new KbpNewsDocParser()
    case "forum" => new KbpForumDocParser()
    case _ => throw new IllegalArgumentException("Unknown corpus type \"%s\"".format(corpus))
  }
  
  def main(args: Array[String]): Unit = {
    
    val inputFile = args(0)
    val corpus = args(1) // web, forum, or news
    
    val docSplitter = new DocSplitter()
    val docParser = getParser(corpus)
    
    val source = io.Source.fromFile(inputFile)
    
    val spliterator = docSplitter.splitDocs(source)
    
    spliterator.take(10).foreach { kbpRawDoc => 
      val text = docParser.parseDoc(kbpRawDoc).get.debugText
      println(text)
    }
  }
}

class KbpForumDocParser extends KbpDocParser {
  override def parseDoc(rawDoc: KbpRawDoc): Option[KbpParsedDoc] = {
    
    var docIdLine = Option.empty[KbpDocLine]
    var textLines = new LinkedList[KbpDocLine]
    
    for (kbpLine <- rawDoc.lines; line = kbpLine.line) {
      
      if (docIdLine.isEmpty && line.startsWith("<doc id")) docIdLine = Some(kbpLine)
      else if (isValidText(line)) textLines.add(kbpLine)
    }
    
    buildDoc(docIdLine, None, None, textLines.asScala.toList)    
  }
}

class KbpNewsDocParser extends KbpDocParser {
  
  override def parseDoc(rawDoc: KbpRawDoc): Option[KbpParsedDoc] = {
    
    var docIdLine = Option.empty[KbpDocLine]
    var dateLine = Option.empty[KbpDocLine]
    var textLines = new LinkedList[KbpDocLine]
    
    var datelineNext = false
    
    for (kbpLine <- rawDoc.lines; line = kbpLine.line) {
      
      if (datelineNext) {
        dateLine = Some(kbpLine)
        datelineNext = false
      } 
      else if (docIdLine.isEmpty && line.startsWith("<DOC id")) docIdLine = Some(kbpLine)
      else if (dateLine.isEmpty && line.startsWith("<DATELINE>")) datelineNext = true
      else if (isValidText(line)) textLines.add(kbpLine)
    }
    
    buildDoc(docIdLine, None, dateLine, textLines.asScala.toList)    
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
      else if (isValidText(line)) textLines.add(kbpLine)
    }
    
    buildDoc(docIdLine, authorLine, dateLine, textLines.asScala.toList)    
  }
}

