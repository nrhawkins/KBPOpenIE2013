package edu.knowitall.tac2013.findSlotFillersApp

import edu.knowitall.tac2013.openie.KbpExtraction

/**
 * Finds fills given candidates for a particular slot.
 */
object SlotFillReranker {

  /*
   * Requires that all candidates (if any) are for the same slot.
   */
  def findAnswers(slotCandidates: Seq[Candidate]): List[Candidate] = {
    
    if (slotCandidates.isEmpty) List.empty
    
    else {
      
      val slot = slotCandidates.head.pattern.slotName
      
      require(slotCandidates.forall(_.pattern.slotName.equals(slot)))
      
      val bestExtr = slotCandidates.maxBy(_.extr.confidence)
      List(new Candidate(bestExtr.pattern, bestExtr.queryType, bestExtr.extr))
    }
  }
}