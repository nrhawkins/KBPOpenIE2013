package edu.knowitall.tac2013.solr.populate

import org.apache.solr.client.solrj.impl.ConcurrentUpdateSolrServer
import edu.knowitall.tac2013.openie._
import edu.knowitall.tac2013.prep._
import java.util.concurrent.atomic.AtomicInteger
import edu.knowitall.tac2013.prep.util.FileUtils
import scopt.OptionParser
import edu.knowitall.tac2013.prep.util.Line
import edu.knowitall.tac2013.prep.util.LineReader
import edu.knowitall.tac2013.solr.KbpExtractionConverter
import scala.Option.option2Iterable
import org.apache.solr.common.SolrInputDocument

class SolrDocumentPathXMLPopulator private (val solrServer: ConcurrentUpdateSolrServer) {

  private val errorCounter = new AtomicInteger(0)
  private val extrCounter = new AtomicInteger(0)

  private def tryAddSingle(docId: String, xml: String): Unit = {
    try {
      addSingle(docId,xml)
    } catch {
      case e: Exception => {
        System.err.println("Error #%d: %s".format(errorCounter.incrementAndGet(), e.getMessage()))
        e.printStackTrace()
      }
    }
  }

  private def addSingle(docId: String, xml: String): Unit = {
    val doc = new SolrInputDocument()
    doc.addField("docId", docId)
    doc.addField("xml", xml)
    
    solrServer.add(doc)
    if (extrCounter.incrementAndGet() % 10000 == 0) {
      System.err.println("SolrPopulator: %d extractions added.".format(extrCounter.get))
    }
  }
  
  private def populate(docs: Iterator[(String,String)]): Unit = {
    
    docs.foreach(f => tryAddSingle(f._1,f._2))
    solrServer.commit()
    solrServer.shutdown()
  }
}

object SolrDocumentPathXMLPopulator {
  
  private val queueSize = 1000
  private val threadCount = 4
  
  private def getDefaultSolrServer(solrUrl: String): ConcurrentUpdateSolrServer = {
    new ConcurrentUpdateSolrServer(solrUrl, queueSize, threadCount)
  }
  
  def populate(extrs: Iterator[(String,String)], solrUrl: String): Unit = {
    val server = getDefaultSolrServer(solrUrl)
    val populator = new SolrDocumentPathXMLPopulator(server)
    populator.populate(extrs)
  }
  
  def main(args: Array[String]): Unit = {
    
    var inputPath = ""
    var solrUrl = "."
    
    val parser = new OptionParser("SolrDocumentPathXMLPopulator") {
      arg("inputPath", "recursive dir.", { s => inputPath = s})
      arg("solrUrl", "URL to Solr instance.", { s => solrUrl = s})
    }

    if (!parser.parse(args)) return
    
    val input =  {
      if (inputPath.equals("")) 
        return
      else {
        val files = FileUtils.getFilesRecursive(new java.io.File(inputPath))
        val pathXMLPairs = files.map(f => (f.getName(),f.getAbsolutePath()))
        pathXMLPairs
      }
    }

    populate(input, solrUrl)
    
  }
}