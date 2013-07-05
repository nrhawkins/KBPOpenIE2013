
package edu.knowitall.tac2013.findSlotFillersApp

import KbpQueryOutput.printUnformattedOutput
import KbpQueryOutput.printFormattedOutput
import KbpQueryOutput.printUnformattedSlotOutput
import KbpQueryOutput.printFormattedSlotOutput
import KBPQueryEntityType._
import edu.knowitall.tac2013.solr.query.SolrQueryExecutor
import java.io.PrintStream
import scopt.OptionParser

//Command line application object for running solr queries on all the slots
//of a given entity and semantic type
object FindSlotFills {

  def main(args: Array[String]): Unit = {

    var entityName = ""
    var entityType = ""
    var slotStrings = ""
    var output = System.out
    
    val parser = new OptionParser() {
      arg("entity name", "The entity's name.", { name => entityName = name })
      arg("entity type", "either organization or person.", { typ => entityType = typ })
      opt("slots", "Specific slots to fill, default = all, comma separated", { slotStrings = _ })
      opt("outputFile", "File name for output. (default stdout)", { file => output = new PrintStream(file) })
    }
    
    if (!parser.parse(args)) return
      
    println(entityName)
    println(entityType)

    val slots = slotStrings.split(",").map(_.trim).filter(_.nonEmpty).toSet
    
    val outputLines = runForServerOutput(entityName, entityType, slots)   
    
    outputLines foreach output.println
    
    if (output != System.out) output.close()
  }

  def runForServerOutput(rawName: String, entityTypeString: String, overrideSlots: Set[String]): Seq[String] = {

    val entityName = rawName.replace("_", " ").trim()
    val entityType = entityTypeString.trim() match {
      case "organization" => ORG
      case "person" => PER
      case _ => throw new IllegalArgumentException("Second Argument must be either 'person' or 'organization'")
    }

    val kbpQuery = if (overrideSlots.isEmpty) {
      KBPQuery.forEntityName(entityName, entityType)
    } else {
      KBPQuery.forEntityName(entityName, entityType).withSlotsToFill(overrideSlots)
    }
    
    val queryExecutor = SolrQueryExecutor.defaultInstance
    
    val unfilteredSlotCandidateSets = kbpQuery.slotsToFill.map { slot => (slot, queryExecutor.executeUnfilteredQuery(kbpQuery, slot)) } toMap
    
    val slotCandidateSets = unfilteredSlotCandidateSets map { case (slot, candidates) => 
      (slot -> FilterSolrResults.filterResults(candidates, entityName)) 
    }
    
    val slotBestAnswers = slotCandidateSets map { case (slot, patternCandidates) =>
      (slot -> SlotFillReranker.findAnswers(kbpQuery, patternCandidates))  
    }

    slotCandidateSets.iterator.toSeq.flatMap { case (slot, candidates) =>
    
    Seq(
      "\n-----------------------------------------\nUNFILTERED RESULTS\n--------------------------------------\n\n",
      printUnformattedSlotOutput(slot, unfilteredSlotCandidateSets(slot)),
      "\n-----------------------------------------\nFILTERED RESULTS\n--------------------------------------\n\n",
      printUnformattedSlotOutput(slot, candidates),
      "\n-----------------------------------------\nFORMATTED RESULTS\n--------------------------------------\n\n",
      printFormattedSlotOutput(slot, kbpQuery, slotBestAnswers(slot)))
    }
  }
}