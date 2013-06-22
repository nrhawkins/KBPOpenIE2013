package edu.knowitall.tac2013.prep

import edu.knowitall.tac2013.prep.KbpSentence
import java.util.concurrent.atomic.AtomicInteger

/**
 * Converts from KbpParsedDoc to KbpSentences
 */
class Sentencer {

  private val errorCounter = new AtomicInteger(0)
  
  /**
   * Returns an empty collection on error.
   */
  def convertToSentences(parsedDoc: KbpParsedDoc): Seq[KbpSentence] = {
    
    val docId = extractDocId(parsedDoc.docIdLine)
    val author = parsedDoc.authorLine flatMap extractAuthor
    val date = parsedDoc.datetimeLine flatMap extractDate
    
    if (docId.isEmpty) {
      val msg = "Sentencer error #%d: Doc skipped; Unable to extract docId from line: %s".format(parsedDoc.docIdLine.line)
      System.err.println(msg)
      Seq.empty
    } else {
      buildKbpSentences(docId.get, author, date, parsedDoc.textLines)
    }
  }
  
  /*
   * Extract docId string, assuming kbpLine contains it.
   */
  private def extractDocId(kbpLine: KbpDocLine): Option[String] = {
    Some("DOCID EXTRACT NOT IMPLEMENTED")
  }
  
  private def extractAuthor(kbpLine: KbpDocLine): Option[String] = {
    Some("AUTHOR EXTRACT NOT IMPLEMENTED")
  }
  
  private def extractDate(kbpLine: KbpDocLine): Option[String] = {
    Some("DATE EXTRACT NOT IMPLEMENTED")
  }
  
  private def buildKbpSentences(docId: String, author: Option[String], date: Option[String], textLines: Seq[KbpDocLine]): Seq[KbpSentence] = {
    throw new Exception("NOT IMPLEMENTED")
  }
}