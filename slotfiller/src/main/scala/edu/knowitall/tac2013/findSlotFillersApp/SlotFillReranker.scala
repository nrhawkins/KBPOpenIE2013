package edu.knowitall.tac2013.findSlotFillersApp

import edu.knowitall.tac2013.openie.KbpExtraction

object SlotFillReranker {

  def findAnswers(slotCandidates: Seq[Candidate]): List[Answer] = {
    
    if (slotCandidates.isEmpty) List.empty
    else {
      val bestExtr = slotCandidates.maxBy(_.extr.confidence)
      List(new Answer(bestExtr.pattern, bestExtr.queryType, bestExtr.extr))
    }
  }
}