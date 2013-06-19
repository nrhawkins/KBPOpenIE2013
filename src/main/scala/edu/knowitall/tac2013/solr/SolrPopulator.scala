package edu.knowitall.tac2013.solr

import org.apache.solr.client.solrj.SolrServer
import org.apache.solr.common.SolrInputDocument
import scala.concurrent.future
import scala.concurrent.ExecutionContext.Implicits.global

abstract class SolrPopulator[E](val solrServer: SolrServer) {

  def extract(line: String): Iterable[E]
  
  def toSolrInputDocument(extr: E): SolrInputDocument
  
  def populateThenCommit(extrs: Iterator[E]): Unit = {
    
    extrs.foreach { extr =>
      val solrDoc = toSolrInputDocument(extr)
      solrServer.add(solrDoc)
    }
    solrServer.commit()
  }
  
  def populateBatch(extrs: Iterator[E], batchSize: Int): Unit = {
    
    extrs.grouped(batchSize) map { group => populateThenCommit(group.iterator) }
  } 
}