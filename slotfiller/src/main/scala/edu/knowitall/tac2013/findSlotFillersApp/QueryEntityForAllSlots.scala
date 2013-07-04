package edu.knowitall.tac2013.findSlotFillersApp

import jp.sf.amateras.solr.scala._
import SingleSolrQueryExecutor.issueSolrQuery
import FilterSolrResults.filterResults
import edu.knowitall.tac2013.openie.KbpExtraction

object QueryEntityForAllSlots {

  //takes entity string and map from KBP slot strings to Open IE relation strings and runs queries
  //to our solr instance for every type of OpenIERelation
  def executeQuery(kbpQuery: KBPQuery): Map[String, List[CandidateSet]] = {

    executeUnfilteredQuery(kbpQuery).map { case (slotname, candidateSets) =>
  
      val filteredCandidateSets = candidateSets.map { candidateSet =>
        val filteredCandidates = filterResults(candidateSet.candidateExtractions, kbpQuery.name)
        new CandidateSet(candidateSet.pattern, filteredCandidates)
      }
      (slotname, filteredCandidateSets)
    }
  }

  //takes entity string and map from KBP slot strings to Open IE relation strings and runs queries
  //to our solr instance for every type of OpenIERelation, this method uses no filters, this is for debugging purposes
  def executeUnfilteredQuery(kbpQuery: KBPQuery): Map[String, List[CandidateSet]] = {

    val patternMap = SlotPattern.patternsForQuery(kbpQuery)
    
    //for every relevant slot 
    for ((slotname, patterns) <- patternMap) yield {
      // aggregate and filter results for every different query formulation
      val resultsList = for (pattern <- patterns) yield {

        val qb = new QueryBuilder(pattern, kbpQuery)
        val combinedResults = qb.getQueries.flatMap { query => issueSolrQuery(query) }
        new CandidateSet(pattern, combinedResults.toList)
      }
      //store list of query formulations and solr results with the string of the slot
      (slotname, resultsList)
    }
  }
}