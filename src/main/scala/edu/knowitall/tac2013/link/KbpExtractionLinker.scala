package edu.knowitall.tac2013.link

import edu.knowitall.browser.entity.EntityLinker
import edu.knowitall.tac2013.openie.KbpExtraction

/**
 * A class to invoke the openie entity linker on KbpExtractions
 * and fill in their wikiLink field. 
 */
class KbpExtractionLinker private (val linker: EntityLinker) {

  /**
   * Returns a new KbpExtraction with (optionally) wikiLink fields
   * filled in.
   */
  def linkExtraction(extr: KbpExtraction): KbpExtraction = {
    
    val context = Seq(extr.sentenceText)
    val arg1String = extr.arg1.originalText
    val arg2String = extr.arg2.originalText
    
    val arg1Link = linker.getBestEntity(arg1String, context)
    val arg2Link = linker.getBestEntity(arg2String, context)
    
    throw new RuntimeException("not implemented")
    
  }
  
}