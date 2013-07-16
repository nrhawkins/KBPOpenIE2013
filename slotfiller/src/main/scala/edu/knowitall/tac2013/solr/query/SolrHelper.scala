package edu.knowitall.tac2013.solr.query

import jp.sf.amateras.solr.scala.SolrClient

object SolrHelper {
  
  val solrUrlForXMLDocsFromOldCorpus = "http://knowitall:knowit!@rv-n16.cs.washington.edu:9325/solr/oldCorpus"
  val solrUrlForXMLDocsFromNewCorpus = "http://knowitall:knowit!@rv-n16.cs.washington.edu:9325/solr/newCorpus"
  val clientForXMLDocsFromOldCorpus = new SolrClient(solrUrlForXMLDocsFromOldCorpus)
  
  
  def getDocIDMapToSentNumsForEntityNameAndNodeID(entityName: String, nodeID: Option[String]) : Map[String,List[(String,Int)]] ={
 
    val client  = SolrQueryExecutor.defaultInstance.solrClient
    
    var solrEntityQueryString = "arg1Text:" + "\"" + entityName + "\" " + "arg2Text:" + "\"" + entityName + "\" "
    if(nodeID.isDefined){
      solrEntityQueryString += "arg1WikiLinkNodeId:" + "\"" + nodeID.get + "\" " + "arg2WikiLinkNodeId:" + "\"" + nodeID.get + "\" "
    }
    
    val docIDQuery = client.query(solrEntityQueryString)
    val result = docIDQuery.rows(1000000).getResultAsMap()
    val docSentNumPairs = for(r <- result.documents) yield {
      (r("docId").toString,r("sentNum").toString().toInt)
    }
    val docIdMapListOfSentNums = docSentNumPairs.groupBy(x => x._1)
    val constrainedMap = docIdMapListOfSentNums.filter(p => (p._2.length > 2))
    val sortedMap = constrainedMap.toList.sortBy(_._2.length)(Ordering[Int].reverse).take(20).toMap
    sortedMap
  }
  
  
  def getRawDoc(docId: String): String = {
    val query = clientForXMLDocsFromOldCorpus.query("docid:\""+ docId + "\"")
    val result = query.getResultAsMap()
    if(result.documents.length != 1){
      throw new Exception("There should be exactly 1 result returned from Solr")
    }
    else{
      result.documents.head("xml").toString
    }
  }

}