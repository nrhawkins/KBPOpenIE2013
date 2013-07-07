package edu.knowitall.tac2013.findSlotFillersApp

import scala.io._
import java.io._
import edu.knowitall.tac2013.openie.KbpExtraction
import edu.knowitall.tac2013.findSlotFillersApp.KBPQueryEntityType._
import edu.knowitall.collection.immutable.Interval
import SlotFillReranker.findAnswers

class OutputFormatter(out: PrintStream) {

  val runID = "UWashington-1"
 
  val indentSize = 4
  
  val maxUnformatted = 12
    
  val indentStr: String = Seq.fill(indentSize)(' ').mkString
  
  def indentStr(i: Int): String = Seq.fill(i)(indentStr).mkString
    
  def println(i: Int, string: String) = out.println(s"${indentStr(i)}$string")
    
  def printUnformattedOutput(mapOfResults: Map[Slot, Seq[Candidate]], kbpQuery: KBPQuery): Unit = {

    for (kbpSlot <- kbpQuery.slotsToFill) yield {
      printUnformattedSlotOutput(kbpSlot, mapOfResults(kbpSlot))
    }
  }

  def printUnformattedSlotOutput(slot: Slot, slotCandidates: Seq[Candidate]): Unit = {

    require(slotCandidates.forall(_.pattern.slotName.equals(slot.name)))

    if (slotCandidates.isEmpty) {
      println(0, s"SLOT NAME: ${slot.name}\n\tNil\n")
    } else {

      println(0,s"SLOT NAME: ${slot.name}")

      val patternCandidatesMap = slotCandidates.groupBy(_.pattern)
      for (pattern <- slot.patterns;
          patternCandidates = patternCandidatesMap.getOrElse(pattern, Nil)) {
        
        val queryCandidatesMap = patternCandidates.groupBy(_.solrQuery)
        println(1, "PATTERN:\t" + pattern.debugString)
        for ((solrQuery, queryCandidates) <- queryCandidatesMap) {

          val topCandidates = queryCandidates.take(maxUnformatted)
          val numTruncated = queryCandidates.size - maxUnformatted
          println(2, "QUERY:\t" + solrQuery.queryString)
          
          for (candidate <- topCandidates) {
            println(3,"arg1: " + candidate.extr.arg1.originalText + "\t rel: " + candidate.extr.rel.originalText +
              "\t arg2: " + candidate.extr.arg2.originalText + "\t docID: " + candidate.extr.sentence.docId +
              "\t confidence: " + candidate.extr.confidence + "\t sentence: " + candidate.extr.sentence.dgraph.text +
              "\t trimFill: " + candidate.trimmedFill.trimmedFillString)
          }
          if (numTruncated > 0) println(3,"(%d more)".format(numTruncated))
          println(0,"")
        }
      }
    }
  }

  /**
   * Overloaded to return a string for server usage
   */
  def printFormattedOutput(bestAnswers: Map[Slot, Seq[Candidate]], kbpQuery: KBPQuery): Unit = {

    //iterate over every slot type
    for (kbpSlot <- kbpQuery.slotsToFill) yield {
      printFormattedSlotOutput(kbpSlot, kbpQuery, bestAnswers.getOrElse(kbpSlot, Nil))
    }
  }

  def printFormattedOutputWithExtraInfo(
    slotCandidateSets: Map[Slot, Seq[Candidate]],
    bestAnswers: Map[Slot, Seq[Candidate]],
    kbpQueryEntityType: KBPQueryEntityType): Unit = {

    for (kbpSlot <- Slot.getSlotTypesList(kbpQueryEntityType)) yield {
      if (slotCandidateSets.contains(kbpSlot)) {
        printFormattedSlotOutputWithExtraInfo(kbpSlot, bestAnswers(kbpSlot))
      }
    } 
  }

  def printFormattedSlotOutput(slot: Slot, kbpQuery: KBPQuery, bestAnswers: Seq[Candidate]): Unit = {

    if (bestAnswers.isEmpty) {
      out.println(Iterator(kbpQuery.id, slot.name, runID, "NIL").mkString("\t"))
    } else {
      for (bestAnswer <- bestAnswers) {
        val queryData = bestAnswer.pattern
        val bestExtr = bestAnswer.extr
        val slotFillIn = queryData.slotFillIn.get.toLowerCase()
        
        require(slotFillIn == "arg1" || slotFillIn == "arg2" || slotFillIn =="relation")
        
        val fields = Iterator(
          kbpQuery.id,
          slot.name,
          runID,
          bestAnswer.extr.sentence.docId,
          bestAnswer.trimmedFill.trimmedFillString,
          bestAnswer.fillOffsetString,
          bestAnswer.entityOffsetString,
          bestAnswer.relOffsetString,
          bestAnswer.extr.confidence)
        
        out.println(fields.mkString("\t"))
      }
    }
  }
  
  def printFormattedSlotOutputWithExtraInfo(slot: Slot, bestAnswers: Seq[Candidate]): Unit = {
    
    if (bestAnswers.isEmpty) {
      out.println(Iterator("queryID", slot, "runID", "NIL").mkString("\t"))
    } else {
      for (bestAnswer <- bestAnswers) {
        val queryData = bestAnswer.pattern
        val bestExtr = bestAnswer.extr
        val slotFillIn = queryData.slotFillIn.get.toLowerCase()
        
        require(slotFillIn == "arg1" || slotFillIn == "arg2" || slotFillIn =="relation")
        
        val fields = Iterator(
          "queryID",
          slot.name,
          "runID",
          bestAnswer.extr.sentence.docId,
          bestAnswer.trimmedFill.trimmedFillString,
          "SlotFill: " + bestAnswer.fillField.originalText + " " +bestAnswer.fillOffsetString,
          "Entity: " + bestAnswer.entityField.originalText + " " + bestAnswer.entityOffsetString,
          "Justification: " + bestAnswer.extr.sentence.dgraph.text + " " +bestAnswer.justificationOffsetString,
          bestAnswer.extr.confidence)
        
        out.println(fields.mkString("\t"))
      }
    }
  }
}

object OutputFormatter {
  val default = new OutputFormatter(System.out) 
}
