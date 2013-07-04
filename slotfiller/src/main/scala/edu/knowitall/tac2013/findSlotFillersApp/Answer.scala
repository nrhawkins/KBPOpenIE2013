package edu.knowitall.tac2013.findSlotFillersApp

import QueryType._
import edu.knowitall.tac2013.openie.KbpExtraction

class Answer(val pattern: SlotPattern, val queryType: CandidateType, val extraction: KbpExtraction)