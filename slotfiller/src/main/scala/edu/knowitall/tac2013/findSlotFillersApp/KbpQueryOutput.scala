package edu.knowitall.tac2013.findSlotFillersApp

import scala.io._
import java.io._
import edu.knowitall.tac2013.openie.KbpExtraction
import edu.knowitall.tac2013.findSlotFillersApp.KBPQueryEntityType._
import edu.knowitall.collection.immutable.Interval

object KbpQueryOutput {

  val runID = "UWashington-1"

  def printUnformattedOutput(mapOfResults: Map[String, Seq[CandidateSet]], kbpQueryEntityType: KBPQueryEntityType): String = {

    val sb = new StringBuilder()
    for (kbpSlot <- SlotTypes.getSlotTypesList(kbpQueryEntityType)) yield {
      
      sb.append("KBP SLOT NAME: " + kbpSlot + "\n")
      
      if (mapOfResults.contains(kbpSlot)) {
        sb.append(printUnformattedSlotOutput(mapOfResults(kbpSlot)))
      } else {
        sb.append("Nil\n")
      }
    }
    sb.toString()
  }

  private def printUnformattedSlotOutput(candidateSets: Seq[CandidateSet]): String = {

    val kbpSlotName = candidateSets.head.pattern.slotName
    
    require(candidateSets.forall(_.pattern.slotName == kbpSlotName), "All candidates must be for the same slot.")
    
    val sb = new StringBuilder

    for (candidateSet <- candidateSets) {

      val pattern = candidateSet.pattern
      val candidateExtractionsList = candidateSet.allExtractions.take(20)

      sb.append("Query pattern:\t" + pattern.debugString)

      sb.append("\tResults:\n")
      if (candidateExtractionsList.length == 0) {
        sb.append("\t\tNil" + "\n")
      }

      for (candidateExtraction <- candidateExtractionsList) {

        sb.append("\t\targ1: " + candidateExtraction.arg1.originalText + "\t rel: " + candidateExtraction.rel.originalText +
          "\t arg2: " + candidateExtraction.arg2.originalText + "\t docID: " + candidateExtraction.sentence.docId +
          "\t confidence: " + candidateExtraction.confidence + "\t sentence: " + candidateExtraction.sentence.dgraph.text + "\n\n")
      }
    }
    return sb.toString
  }

  def printFormattedOutput(
    slotCandidateSets: Map[String, Seq[CandidateSet]],
    bestAnswers: Map[String, List[Answer]],
    filePath: String, kbpQueryEntityType: KBPQueryEntityType) {

    val writer = new PrintWriter(new File(filePath))

    //iterate over every slot type
    for (kbpSlot <- SlotTypes.getSlotTypesList(kbpQueryEntityType)) {

      //if the kbp slot is contained in the results
      if (slotCandidateSets.contains(kbpSlot)) {

        //for each slot print one response for single-valued slot
        //print k-slots for multi-valued slot
        //or print NIL

        val kbpSlotName = kbpSlot
        val candidateSets = slotCandidateSets(kbpSlot)

        val bestAnswerExtractions = bestAnswers(kbpSlot)

        if (!bestAnswerExtractions.isEmpty) {
          val bestAnswer = bestAnswerExtractions.head
          val queryData = bestAnswer.pattern
          val slotFiller = {
            if (queryData.slotFillIn.get.toLowerCase().trim() == "arg1") {
              bestAnswer.extraction.arg1.originalText
            } else if (queryData.slotFillIn.get.toLowerCase().trim() == "arg2") {
              bestAnswer.extraction.arg2.originalText
            }
          }

          val fillerOffset = {
            if (queryData.slotFillIn.get.toLowerCase().trim() == "arg1") {
              bestAnswer.extraction.arg1.tokenInterval
            } else if (queryData.slotFillIn.get.toLowerCase().trim() == "arg2") {
              bestAnswer.extraction.arg2.tokenInterval
            }
          }

          val entityOffset = {
            if (queryData.entityIn.get.toLowerCase().trim() == "arg1") {
              bestAnswer.extraction.arg1.tokenInterval
            } else if (queryData.entityIn.get.toLowerCase().trim() == "arg2") {
              bestAnswer.extraction.arg2.tokenInterval
            }
          }

          writer.write(Iterator("queryID", kbpSlot, "runID", bestAnswer.extraction.sentence.docId, slotFiller,
            fillerOffset, entityOffset, bestAnswer.extraction.rel.tokenInterval,
            bestAnswer.extraction.confidence).mkString("\t") + "\n")

        } else {
          writer.write(Iterator("queryID", kbpSlot, "runID", "NIL").mkString("\t") + "\n")
        }
      } else {
        // else if the results Map does not contain the slot
        // print nothing since this slot is ignored

      }

    }

    writer.close()
  }

  /**
   * Overloaded to return a string for server usage
   */
  def printFormattedOutput(
    slotCandidateSets: Map[String, Seq[CandidateSet]],
    bestAnswers: Map[String, List[Answer]],
    kbpQueryEntityType: KBPQueryEntityType): String = {

    val sb = new StringBuilder()

    //iterate over every slot type
    for (kbpSlot <- SlotTypes.getSlotTypesList(kbpQueryEntityType)) {

      //if the kbp slot is contained in the results
      if (slotCandidateSets.contains(kbpSlot)) {

        //for each slot print one response for single-valued slot
        //print k-slots for multi-valued slot
        //or print NIL

        val kbpSlotName = kbpSlot
        val candidateSets = slotCandidateSets(kbpSlot)

        val bestAnswerExtractions = bestAnswers(kbpSlot)

        if (!bestAnswerExtractions.isEmpty) {
          val bestAnswer = bestAnswerExtractions.head
          val queryData = bestAnswer.pattern
          val slotFiller = {
            if (queryData.slotFillIn.get.toLowerCase().trim() == "arg1") {
              bestAnswer.extraction.arg1.originalText
            } else if (queryData.slotFillIn.get.toLowerCase().trim() == "arg2") {
              bestAnswer.extraction.arg2.originalText
            }
          }

          val fillerOffset = {
            if (queryData.slotFillIn.get.toLowerCase().trim() == "arg1") {
              bestAnswer.extraction.arg1.tokenInterval
            } else if (queryData.slotFillIn.get.toLowerCase().trim() == "arg2") {
              bestAnswer.extraction.arg2.tokenInterval
            }
          }

          val entityOffset = {
            if (queryData.entityIn.get.toLowerCase().trim() == "arg1") {
              bestAnswer.extraction.arg1.tokenInterval
            } else if (queryData.entityIn.get.toLowerCase().trim() == "arg2") {
              bestAnswer.extraction.arg2.tokenInterval
            }
          }

          sb.append(Iterator("queryID", kbpSlot, "runID", bestAnswer.extraction.sentence.docId, slotFiller,
            fillerOffset, entityOffset, bestAnswer.extraction.rel.tokenInterval,
            bestAnswer.extraction.confidence).mkString("\t") + "\n")

        } else {
          sb.append(Iterator("queryID", kbpSlot, "runID", "NIL").mkString("\t") + "\n")
        }
      } else {
        // else if the results Map does not contain the slot
        // print nothing since this slot is ignored

      }

    }

    sb.toString
  }

  def printFormattedOutputForKBPQuery(
    slotCandidateSets: Map[String, Seq[CandidateSet]],
    bestAnswers: Map[String, List[Answer]],
    filePath: String, kbpQuery: KBPQuery) {

    val writer = new FileWriter(new File(filePath), true)

    //iterate over every slot type
    for (kbpSlot <- SlotTypes.getSlotTypesList(kbpQuery.entityType)) {

      //if the kbp slot is contained in the results
      if (slotCandidateSets.contains(kbpSlot)) {

        //for each slot print one response for single-valued slot
        //print k-slots for multi-valued slot
        //or print NIL

        val kbpSlotName = kbpSlot
        val candidateSets = slotCandidateSets(kbpSlot)

        //for now assume every slot is single valued, use a
        //separate filter method to choose best answer

        val bestAnswerExtractions = bestAnswers(kbpSlot)

        if (!bestAnswerExtractions.isEmpty) {
          val bestAnswer = bestAnswerExtractions.head
          val queryData = bestAnswer.pattern
          val slotFiller = {
            if (queryData.slotFillIn.get.toLowerCase().trim() == "arg1") {
              bestAnswer.extraction.arg1.originalText
            } else if (queryData.slotFillIn.get.toLowerCase().trim() == "arg2") {
              bestAnswer.extraction.arg2.originalText
            }
          }

          val fillerOffsetInterval = {
            if (queryData.slotFillIn.get.toLowerCase().trim() == "arg1") {
              bestAnswer.extraction.arg1.tokenInterval
            } else if (queryData.slotFillIn.get.toLowerCase().trim() == "arg2") {
              bestAnswer.extraction.arg2.tokenInterval
            } else if (queryData.slotFillIn.get.toLowerCase().trim() == "relation") {
              bestAnswer.extraction.rel.tokenInterval
            } else {
              throw new Exception("slotFillIn specification should only be arg1, arg2, or relation")
            }
          }

          val entityOffsetInterval = {
            if (queryData.entityIn.get.toLowerCase().trim() == "arg1") {
              bestAnswer.extraction.arg1.tokenInterval
            } else if (queryData.entityIn.get.toLowerCase().trim() == "arg2") {
              bestAnswer.extraction.arg2.tokenInterval
            } else {
              throw new Exception("entityIn specification should only be arg1 or arg2")
            }
          }

          val startOffset = bestAnswer.extraction.sentence.startOffset

          val fillerOffsetStart = startOffset + fillerOffsetInterval.start
          val fillerOffsetEnd = startOffset + fillerOffsetInterval.end
          val fillerOffsetString = fillerOffsetStart.toString + "-" + fillerOffsetEnd.toString()

          val entityOffsetStart = startOffset + entityOffsetInterval.start
          val entityOffsetEnd = startOffset + entityOffsetInterval.end
          val entityOffsetString = entityOffsetStart.toString + "-" + entityOffsetEnd.toString()

          val justificationOffsetStart = startOffset + bestAnswer.extraction.arg1.tokenInterval.start
          val justificationOffsetEnd = startOffset + bestAnswer.extraction.arg2.tokenInterval.end
          val justificationOffsetString = justificationOffsetStart.toString() + "-" + justificationOffsetEnd.toString()

          writer.write(Iterator(kbpQuery.id, kbpSlot, runID, bestAnswer.extraction.sentence.docId, slotFiller,
            fillerOffsetString, entityOffsetString, justificationOffsetString, bestAnswer.extraction.confidence).mkString("\t") + "\n")

        } else {
          //no answer found
          writer.write(Iterator(kbpQuery.id, kbpSlot, runID, "NIL").mkString("\t") + "\n")
        }

      } else {
        // else if the results Map does not contain the slot
        // print nothing since this slot is ignored

      }

    }

    writer.close()
  }

}