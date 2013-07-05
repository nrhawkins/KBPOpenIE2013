package edu.knowitall.tac2013.findSlotFillersApp

import QueryType._
import edu.knowitall.tac2013.openie.KbpExtraction
import edu.knowitall.tac2013.openie.KbpArgument
import edu.knowitall.tac2013.openie.KbpExtractionField
import edu.knowitall.tac2013.openie.WikiLink
import edu.knowitall.tool.chunk.ChunkedToken

class Candidate(val pattern: SlotPattern, val queryType: QueryType, val extr: KbpExtraction) {
  
    /**
   * Concatenates tokens from (arg1, rel, arg2) which are nouns, pronouns, or verbs
   * If arg1 or arg2 is linked, uses fbid for that field instead.
   */
  def extractionKey: String = {
    
    def tokenKey(tokens: TraversableOnce[ChunkedToken]): String = {
      tokens.filter(tok => tok.isNoun || tok.isPronoun || tok.isVerb).map(_.string).mkString(" ")
    }
    
    def argKey(arg: KbpArgument) = arg.wikiLink match {
      case Some(WikiLink(name, fbid, nodeIdOpt)) => fbid
      case None => tokenKey(arg.tokens)
    }

    val arg1Key = argKey(extr.arg1)
    val relKey  = tokenKey(extr.rel.tokens)
    val arg2Key = argKey(extr.arg2)
    
    Seq(arg1Key, relKey, arg2Key).mkString(", ")
  }
}