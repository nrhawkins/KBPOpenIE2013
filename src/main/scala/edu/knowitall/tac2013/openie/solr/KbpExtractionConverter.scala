package edu.knowitall.tac2013.openie.solr

import org.apache.solr.common.SolrInputDocument
import edu.knowitall.tac2013.openie.KbpExtraction
import scala.collection.JavaConverters._

/**
 * Helper object for converting between KbpExtractions and SolrDocuments
 */
object KbpExtractionConverter {

  val kbpExtractionFields = Seq(
      "arg1Text",
      "arg1Interval",
      "arg1WikiLink",
      "arg1Types",
      
      "relText",
      "relInterval",
      
      "arg2Text",
      "arg2Interval",
      "arg2WikiLink",
      "arg2Types",
      
      "confidence",
      "extractor",
      // sentence fields
      "docId",
      "sentOffset",
      "sentNum",
      "chunks",
      "dgraph"
    )
  
  def toSolrInputDocument(extr: KbpExtraction): SolrInputDocument = {
    
    // Prepare fields as strings
    
    val arg1 = extr.arg1
    val rel = extr.rel
    val arg2 = extr.arg2
    val sent = extr.sentence
    
    val arg1Text = extr.arg1.originalText
    val arg1Interval = "%d %d".format(arg1.tokenInterval.start, arg1.tokenInterval.last)
    val arg1WikiLink = arg1.wikiLink.getOrElse("")
    val arg1Types = arg1.types.mkString(" ")
    
    val relText = extr.rel.originalText
    val relInterval = "%d %d".format(rel.tokenInterval.start, rel.tokenInterval.last)
    
    val arg2Text = extr.arg2.originalText
    val arg2Interval = "%d %d".format(arg2.tokenInterval.start, arg2.tokenInterval.last)
    val arg2WikiLink = arg2.wikiLink.getOrElse("")
    val arg2Types = arg2.types.mkString(" ")
    
    val confidence = "%.04f".format(extr.confidence)
    val extractor = extr.extractor
    
    val docId = sent.docId
    val sentOffset = sent.startOffset
    val sentNum = sent.sentNum
    val chunks = sent.chunks.mkString(" ")
    val dgraph = sent.dgraph.serialize
    
    // Insert fields into doc
    
    val doc = new SolrInputDocument()
    doc.addField("arg1Text", arg1Text)
    doc.addField("arg1Interval", arg1Interval)
    doc.addField("arg1WikiLink", arg1WikiLink)
    doc.addField("arg1Types", arg1Types)
    
    doc.addField("relText", relText)
    doc.addField("relInterval", relInterval)
    
    doc.addField("arg2Text", arg2Text)
    doc.addField("arg2Interval", arg2Interval)
    doc.addField("arg2WikiLink", arg2WikiLink)
    doc.addField("arg2Types", arg2Types)
    
    doc.addField("confidence", confidence)
    doc.addField("extractor", extractor)
    
    doc.addField("docId", docId)
    doc.addField("sentOffset", sentOffset)
    doc.addField("sentNum", sentNum)
    doc.addField("chunks", chunks)
    doc.addField("dgraph", dgraph)
    
    assert(kbpExtractionFields.toSeq.equals(doc.getFieldNames().asScala.toSet))
    
    doc
  }
  
}