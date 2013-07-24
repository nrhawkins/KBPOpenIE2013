package edu.knowitall.tac2013.experiment

import jp.sf.amateras.solr.scala.SolrClient

object DocumentSizeData {
  
  def main(args: Array[String]){
    
    val solrUrl = "http://knowitall:knowit!@rv-n16.cs.washington.edu:9325/solr/newCorpus"
    val client = new SolrClient(solrUrl)
    val allQuery = client.query("*:*")
    val result = allQuery.rows(3000000).getResultAsMap()
//    
//    var from0to5000chars = 0
//    var from5000to10000chars = 0
//    var from10000to15000chars = 0
//    var from15000to20000chars = 0
//    var above20000chars = 0


    
    
    
    for(r <- result.documents){
      
      
    }
    
    
    
  }

}