package edu.knowitall.tac2013.findSlotFillersApp

import edu.knowitall.tac2013.openie.KbpExtraction

class CandidateExtraction (val kbpExtraction: KbpExtraction, val candidateType: CandidateType.Value, val pattern: KbpSlotToOpenIEData){
  
  var bestSlotFillString: Option[String] = None
  
  
  def setBestSlotFillString(str:String){
    bestSlotFillString = Some(str)
  }
  

}