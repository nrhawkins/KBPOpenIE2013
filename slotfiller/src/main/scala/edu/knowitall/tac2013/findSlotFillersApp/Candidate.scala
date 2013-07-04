package edu.knowitall.tac2013.findSlotFillersApp

import QueryType._
import edu.knowitall.tac2013.openie.KbpExtraction

class Candidate(val pattern: SlotPattern, val queryType: QueryType, val extr: KbpExtraction)