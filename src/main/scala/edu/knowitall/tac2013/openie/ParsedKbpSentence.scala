package edu.knowitall.tac2013.openie

import edu.knowitall.tool.parse.ParseTree

case class ParsedKbpSentence(
    val docId: String, 
    val sentId: Int, 
    val text: String, 
    val tokens: String, 
    val postags: String, 
    val chunks: String, 
    val dgraph: String)
    
object ParsedKbpSentence {
  
  import KbpSentence.tabRegex
  
  def read(pickle: String): Option[ParsedKbpSentence] = {
    tabRegex.split(pickle) match {
      case Array(docId, sentIdString, text, tokens, postags, chunks, dgraph, _*) => 
        Some(ParsedKbpSentence(docId, sentIdString.toInt, text, tokens, postags, chunks, dgraph))
      case _ => {
        System.err.println("Error reading ParsedKbpSentence: %s".format(pickle))
        None
      }
    }
  }
  
  def write(parsedKbpSentence: ParsedKbpSentence): String = {
    ParsedKbpSentence.unapply(parsedKbpSentence).get.productIterator.map(_.toString).mkString("\t")
  }
}