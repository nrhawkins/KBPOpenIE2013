package edu.knowitall.tac2013.findSlotFillersApp

import CandidateType._
import edu.knowitall.tac2013.openie.KbpExtraction

class CandidateSet(val pattern: SlotPattern, val extractionsMap: Map[CandidateType, Seq[KbpExtraction]]) {
  
  val allExtractions = extractionsMap.values.toSeq.flatten
  
  def extractionsFrom(extrType: CandidateType) = extractionsMap.getOrElse(extrType, Seq.empty)
  
  var rankedAnswers = List[KbpExtraction]()
  
  def setRankedAnswers(newRankedAnswers: List[KbpExtraction]) { rankedAnswers = newRankedAnswers }
}