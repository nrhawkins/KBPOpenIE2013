
package edu.knowitall.tac2013.findSlotFillersApp

import KbpQueryOutput.printUnformattedOutput
import KbpQueryOutput.printFormattedOutput
import SlotFillReranker.chooseBestTest
import KBPQueryEntityType._
import edu.knowitall.tac2013.solr.query.SolrQueryExecutor

//Command line application object for running solr queries on all the slots
//of a given entity and semantic type
object FindSlotFills {

  def main(args: Array[String]) {

    assert(args.length == 3,
      "there should be three arguments: entity name, semantic type (organization or person), and file path for output")

    val entityName = args(0).replace("_", " ")
    val entityType = args(1) match {
      case "organization" => ORG
      case "person" => PER
      case _ => throw new IllegalArgumentException("Second Argument must be either 'person' or 'organization'")
    }
    println(entityName)
    println(args(1))

    val kbpQuery = KBPQuery.forEntityName(entityName, entityType)

    val queryExecutor = SolrQueryExecutor.defaultInstance
    
    val slots = SlotPattern.patternsForQuery(kbpQuery).keySet
    
    val slotCandidateSets = slots map { slot => (slot, queryExecutor.executeQuery(kbpQuery, slot)) } toMap

    val slotBestAnswers = slotCandidateSets map { case (slot, patternCandidates) =>
      (slot, chooseBestTest(patternCandidates))  
    } toMap
    

    printUnformattedOutput(slotCandidateSets, args(2), kbpQuery.entityType)
    printFormattedOutput(slotCandidateSets, slotBestAnswers, args(2), kbpQuery.entityType)
  }

  def runForServerOutput(field1: String, field2: String, nodeId: Option[String] = None): String = {

    val entityName = field1.replace("_", " ").trim()
    val entityType = field2.trim() match {
      case "organization" => ORG
      case "person" => PER
      case _ => throw new IllegalArgumentException("Second Argument must be either 'person' or 'organization'")
    }

    val kbpQuery = KBPQuery.forEntityName(entityName, entityType)
    
    val queryExecutor = SolrQueryExecutor.defaultInstance
    
    val slots = SlotPattern.patternsForQuery(kbpQuery).keySet
    
    val slotCandidateSets = slots map { slot => (slot, queryExecutor.executeQuery(kbpQuery, slot)) } toMap

    val unfilteredSlotCandidateSets = slots map { slot => (slot, queryExecutor.executeUnfilteredQuery(kbpQuery, slot)) } toMap
    
    val slotBestAnswers = slotCandidateSets map { case (slot, patternCandidates) =>
      (slot, chooseBestTest(patternCandidates))  
    } toMap

    return (
      "\n-----------------------------------------\nUNFILTERED RESULTS\n--------------------------------------\n\n" +
      printUnformattedOutput(unfilteredSlotCandidateSets, kbpQuery.entityType) +
      "\n-----------------------------------------\nFILTERED RESULTS\n--------------------------------------\n\n" +
      printUnformattedOutput(slotCandidateSets, kbpQuery.entityType) +
      "\n-----------------------------------------\nFORMATTED RESULTS\n--------------------------------------\n\n" +
      printFormattedOutput(slotCandidateSets, slotBestAnswers, kbpQuery.entityType))
  }
}