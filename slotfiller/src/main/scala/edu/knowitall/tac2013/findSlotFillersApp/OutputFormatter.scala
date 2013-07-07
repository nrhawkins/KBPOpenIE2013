package edu.knowitall.tac2013.findSlotFillersApp

import scala.io._
import java.io._
import edu.knowitall.tac2013.solr.query.SolrQuery
import edu.knowitall.tac2013.openie.KbpExtraction
import edu.knowitall.tac2013.findSlotFillersApp.KBPQueryEntityType._
import edu.knowitall.collection.immutable.Interval


class OutputFormatter(out: PrintStream) {

  val runID = "UWashington-1"
 
  val indentSize = 4
  
  val maxUnformatted = 12
  
  val doubleSpace = false
  
  val detailedCandidates = true
  val detailedAnswers = true
  
  val printFiltered = true
  val printUnfiltered = true
    
  val indentStr: String = Seq.fill(indentSize)(' ').mkString
  
  private def indentStr(i: Int): String = Seq.fill(i)(indentStr).mkString
    
  private def println(i: Int, string: String) = {
    out.println(s"${indentStr(i)}$string")
    if (doubleSpace) out.println
  }
    
  private def printSolrResults(resultsName: String, mapOfResults: Map[Slot, Seq[Candidate]], kbpQuery: KBPQuery): Unit = {

    println(0, "")
    println(0, s"------------------- $resultsName RESULTS -------------------")
    println(0, "")
    
    for (kbpSlot <- kbpQuery.slotsToFill) {
      printSlotOutput(kbpSlot, mapOfResults(kbpSlot))
    }
  }
  
  def printFilteredResults(mapOfResults: Map[Slot, Seq[Candidate]], kbpQuery: KBPQuery): Unit = {
    if (printFiltered) printSolrResults("FILTERED", mapOfResults, kbpQuery)
  }
  
  def printUnfilteredResults(mapOfResults: Map[Slot, Seq[Candidate]], kbpQuery: KBPQuery): Unit = { 
    if (printUnfiltered) printSolrResults("UNFILTERED", mapOfResults, kbpQuery)
  }
  
  private def printSlotOutput(slot: Slot, slotCandidates: Seq[Candidate]): Unit = {

    require(slotCandidates.forall(_.pattern.slotName.equals(slot.name)))

    println(0, s"(${slotCandidates.size}) ${slot.name}")

    val patternCandidatesMap = slotCandidates.groupBy(_.pattern)
    if (slotCandidates.size > 0) for (
      pattern <- slot.patterns;
      patternCandidates = patternCandidatesMap.getOrElse(pattern, Nil)
    ) {

      printPatternOutput(pattern, patternCandidates)
    }
    println(0, "")
  }

  private def printPatternOutput(pattern: SlotPattern, patternCandidates: Seq[Candidate]): Unit = {
    val queryCandidatesMap = patternCandidates.groupBy(_.solrQuery)
    println(1, "(%d) ".format(patternCandidates.size) + pattern.debugString)
    for ((solrQuery, queryCandidates) <- queryCandidatesMap) {

      printQueryOutput(solrQuery, queryCandidates)
    }
  }

  private def printQueryOutput(solrQuery: SolrQuery, queryCandidates: Seq[Candidate]): Unit = {
    val topCandidates = queryCandidates.take(maxUnformatted)
    val numTruncated = queryCandidates.size - maxUnformatted
    println(2, "(%d) ".format(queryCandidates.size) + solrQuery.queryString)

    if (detailedCandidates) {
      for (candidate <- topCandidates) {
        println(3, candidate.debugString)
      }
      if (numTruncated > 0) println(3, "(%d more)".format(numTruncated))
    }
  }
  
  /**
   * Overloaded to return a string for server usage
   */
  def printAnswers(bestAnswers: Map[Slot, Seq[Candidate]], kbpQuery: KBPQuery): Unit = {

    println(0, "")
    println(0, s"------------------- FORMATTED SLOT FILLS -------------------")
    println(0, "")
    
    //iterate over every slot type
    for (kbpSlot <- kbpQuery.slotsToFill) yield {
      if (detailedAnswers) printDetailedSlotAnswer(kbpSlot, kbpQuery, bestAnswers.getOrElse(kbpSlot, Nil))
      else printSlotAnswer(kbpSlot, kbpQuery, bestAnswers.getOrElse(kbpSlot, Nil))
    }
  }

  private def printSlotAnswer(slot: Slot, kbpQuery: KBPQuery, bestAnswers: Seq[Candidate]): Unit = {

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
          kbpQuery.name,
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
  
  private def printDetailedSlotAnswer(slot: Slot, kbpQuery: KBPQuery, bestAnswers: Seq[Candidate]): Unit = {
    
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
          "Fill: " + bestAnswer.fillField.originalText + " " +bestAnswer.fillOffsetString,
          "Entity: " + bestAnswer.entityField.originalText + " " + bestAnswer.entityOffsetString,
          "Just: " + bestAnswer.extr.sentence.dgraph.text + " " +bestAnswer.justificationOffsetString,
          bestAnswer.extr.confidence)
        
        out.println(fields.mkString("\t"))
      }
    }
  }
}

object OutputFormatter {
  val default = new OutputFormatter(System.out) 
}
