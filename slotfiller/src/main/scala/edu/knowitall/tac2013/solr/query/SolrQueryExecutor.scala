package edu.knowitall.tac2013.solr.query

import jp.sf.amateras.solr.scala._
import edu.knowitall.tac2013.findSlotFillersApp.FilterSolrResults.filterResults
import edu.knowitall.tac2013.openie.KbpExtraction
import edu.knowitall.tac2013.findSlotFillersApp.CandidateExtraction
import edu.knowitall.tac2013.findSlotFillersApp.CandidateSet
import edu.knowitall.tac2013.findSlotFillersApp.KBPQuery
import edu.knowitall.tac2013.findSlotFillersApp.SlotPattern
import scala.Option.option2Iterable

class SolrQueryExecutor(val solrClient: SolrClient) {
  
  def this(url: String) = this(new SolrClient(url))
  
  private def issueSolrQuery(kbpSolrQuery: SolrQuery): List[CandidateExtraction] = {
    //not sure where the best place to put this val is so I'm hoping making it lazy
    //will be a good idea
    
    println(kbpSolrQuery.queryString)
    
    val query = solrClient.query(kbpSolrQuery.queryString)
    val result = query.sortBy("confidence",Order.desc).rows(10000).getResultAsMap()

    val extrs = result.documents.flatMap { doc =>
      val fieldMap = doc.asInstanceOf[Map[String, Any]]
      KbpExtraction.fromFieldMap(fieldMap) match {
        case Some(x) => { Option(new CandidateExtraction(x, kbpSolrQuery.resultType, kbpSolrQuery.pattern)) }
        case None => { None }
      }
    }
    extrs
  }
  
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

        val qb = new SolrQueryBuilder(pattern, kbpQuery)
        val combinedResults = qb.getQueries.flatMap { query => issueSolrQuery(query) }
        new CandidateSet(pattern, combinedResults.toList)
      }
      //store list of query formulations and solr results with the string of the slot
      (slotname, resultsList)
    }
  }
}

object SolrQueryExecutor {
  
  val defaultSolrUrl = "http://knowitall:knowit!@rv-n16.cs.washington.edu:9321/solr"
    
  lazy val defaultInstance = new SolrQueryExecutor(defaultSolrUrl) 
}