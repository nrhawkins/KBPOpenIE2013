package edu.knowitall.tac2013.openie

import edu.knowitall.tool.chunk.ChunkedToken


case class ParsedKbpSentence(
    val docId: String, 
    val tokens: String, 
    val postags: String, 
    val chunks: String, 
    val offsets: String,
    val dgraph: String) {
  
  import ParsedKbpSentence.wsSplit
  
  def chunkedTokens = {
    
    val ts = wsSplit.split(tokens)
    val ps = wsSplit.split(postags)
    val cs = wsSplit.split(chunks)
    val os = wsSplit.split(offsets)
    
    ts.zip(ps).zip(cs).zip(os).map { case  (((token, postag), chunk), offset) =>
      new ChunkedToken(chunk, postag, token, offset.toInt)
    }
  }
  
}
    
object ParsedKbpSentence {
  
  private val wsSplit = "\\s+".r
  
  val NUM_FIELDS = 6
  
  import KbpSentence.tabRegex
  
  def read(pickle: String): Option[ParsedKbpSentence] = read(tabRegex.split(pickle))
  
  def read(split: Array[String]): Option[ParsedKbpSentence] = {
    split match {
      case Array(docId, tokens, postags, chunks, offsets, dgraph, _*) => 
        Some(ParsedKbpSentence(docId, tokens, postags, chunks, offsets, dgraph))
      case _ => {
        System.err.println("Error reading ParsedKbpSentence: %s".format(split.mkString("\t")))
        None
      }
    }
  }
  
  def write(parsedKbpSentence: ParsedKbpSentence): String = {
    ParsedKbpSentence.unapply(parsedKbpSentence).get.productIterator.map(_.toString).mkString("\t")
  }
}