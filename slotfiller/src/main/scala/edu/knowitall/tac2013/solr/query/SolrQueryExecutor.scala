package edu.knowitall.tac2013.solr.query

import jp.sf.amateras.solr.scala._
import edu.knowitall.tac2013.findSlotFillersApp.FilterSolrResults.filterResults
import edu.knowitall.tac2013.openie.KbpExtraction
import edu.knowitall.tac2013.findSlotFillersApp.CandidateSet
import edu.knowitall.tac2013.findSlotFillersApp.KBPQuery
import edu.knowitall.tac2013.findSlotFillersApp.SlotPattern
import scala.Option.option2Iterable

class SolrQueryExecutor(val solrClient: SolrClient) {
  
  def this(url: String) = this(new SolrClient(url))
  
  private def issueSolrQuery(kbpSolrQuery: SolrQuery): List[KbpExtraction] = {
    
    println(kbpSolrQuery.queryString)
    
    val query = solrClient.query(kbpSolrQuery.queryString)
    val result = query.sortBy("confidence",Order.desc).rows(10000).getResultAsMap()

    val extrs = result.documents.flatMap { doc =>
      val fieldMap = doc.asInstanceOf[Map[String, Any]]
      KbpExtraction.fromFieldMap(fieldMap)
    }
    extrs
  }
  
  //takes entity string and map from KBP slot strings to Open IE relation strings and runs queries
  //to our solr instance for every type of OpenIERelation
  def executeQuery(kbpQuery: KBPQuery, slot: String): List[CandidateSet] = {

    val unfilteredCandidates = executeUnfilteredQuery(kbpQuery, slot)

    val filteredCandidateSets = unfilteredCandidates.map { candidateSet => filterResults(candidateSet, kbpQuery.name) }

    filteredCandidateSets
  }

  //takes entity string and map from KBP slot strings to Open IE relation strings and runs queries
  //to our solr instance for every type of OpenIERelation, this method uses no filters, this is for debugging purposes
  def executeUnfilteredQuery(kbpQuery: KBPQuery, slot: String): List[CandidateSet] = {

    val patterns = SlotPattern.patternsForQuery(kbpQuery)(slot)

    // aggregate and filter results for every different query formulation
    val resultsList = for (pattern <- patterns) yield {

      val qb = new SolrQueryBuilder(pattern, kbpQuery)
      val combinedResults = qb.getQueries.map { query => (query.resultType, issueSolrQuery(query)) } toMap
      
      new CandidateSet(pattern, combinedResults)
    }
    //store list of query formulations and solr results with the string of the slot
    resultsList
  }
}

object SolrQueryExecutor {
  
  val defaultSolrUrl = "http://knowitall:knowit!@rv-n16.cs.washington.edu:9321/solr"
    
  lazy val defaultInstance = new SolrQueryExecutor(defaultSolrUrl) 
}