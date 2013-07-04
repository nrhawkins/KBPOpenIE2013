package edu.knowitall.tac2013.findSlotFillersApp

import edu.knowitall.tac2013.openie.KbpExtraction

object SlotFillReranker {

  def findAnswers(candidateSets: List[CandidateSet]): List[Answer] = {
    
    val expandedResults = candidateSets.flatMap({ candidateSet =>
      QueryType.values.toSeq.flatMap({ queryType =>
        candidateSet.extractionsFrom(queryType).map { extr => (candidateSet, queryType, extr) }
      })
    })
    if (expandedResults.isEmpty) List.empty
    else {
      val (bestCandidateSet, bestQueryType, bestExtr) = expandedResults.maxBy(_._3.confidence)
      List(new Answer(bestCandidateSet.pattern, bestQueryType, bestExtr))
    }
  }
}