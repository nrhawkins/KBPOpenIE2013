package edu.knowitall.tac2013.app.util

import edu.knowitall.tac2013.app.Candidate
import edu.knowitall.tac2013.stanford.annotator.utils.StanfordAnnotatorHelperMethods
import edu.knowitall.tac2013.app.Slot

object DateUtils {
  
  val stanfordHelper = new StanfordAnnotatorHelperMethods()
  
  def putInTimexFormat(slotCandidates: Map[String,Seq[Candidate]]): Map[String,Seq[Candidate]] = {
    
    for(slot <- slotCandidates.keys){
      if(Slot.fromName(slot).isDate){
        //get timex format from SUTime
        val candidates = slotCandidates(slot)
        for(candidate <- candidates){
          val timexFormattedFill = stanfordHelper.getNormalizedDate(candidate.trimmedFill.interval, candidate.extr.sentence.docId ,candidate.trimmedFill.string)
        }
        
      }
    }
    
  }

}