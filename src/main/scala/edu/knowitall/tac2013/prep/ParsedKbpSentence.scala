package edu.knowitall.tac2013.prep

import edu.knowitall.tool.chunk.ChunkedToken
import edu.knowitall.tool.chunk.Chunker
import edu.knowitall.tool.tokenize.Tokenizer
import KbpSentence.tabRegex
import scala.Array.canBuildFrom


case class ParsedKbpSentence(
    val docId: String, 
    val tokens: String, 
    val postags: String, 
    val chunks: String, 
    val offset: String,
    val dgraph: String) {
  
  import ParsedKbpSentence.wsSplit
  
  def chunkedTokens = {
    
    val ts = Tokenizer.computeOffsets(wsSplit.split(tokens), tokens)
    val ps = wsSplit.split(postags)
    val cs = wsSplit.split(chunks)
    
    Chunker.tokensFrom(cs, ps, ts)
  }
  
}
    
object ParsedKbpSentence {
  
  private val wsSplit = "\\s+".r
  
  val NUM_FIELDS = 6
  
  import KbpSentence.tabRegex
  
  def read(pickle: String): Option[ParsedKbpSentence] = read(tabRegex.split(pickle))
  
  def read(split: Array[String]): Option[ParsedKbpSentence] = {
    split match {
      case Array(docId, tokens, postags, chunks, offset, dgraph, _*) => 
        Some(ParsedKbpSentence(docId, tokens, postags, chunks, offset, dgraph))
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