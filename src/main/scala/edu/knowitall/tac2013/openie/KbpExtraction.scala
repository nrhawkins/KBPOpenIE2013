package edu.knowitall.tac2013.openie

import edu.knowitall.srlie.SrlExtractionInstance

case class KbpExtraction(
  val arg1: String,
  val rel: String,
  val arg2: String,
  val arg1postags: String,
  val relpostags: String,
  val arg2postags: String,
  val docId: String,
  val sentId: String,
  val sent: String,
  val sentStartOffset: String,
  val sentEndOffset: String,
  val confidence: String,
  val extractor: String
)

object KbpExtraction {
  
  def toString(e: KbpExtraction): String = {
    val fields = Seq(e.arg1, e.rel, e.arg2, e.arg1postags, e.relpostags, e.arg2postags, e.docId, e.sentId, e.sent, e.sentStartOffset, e.sentEndOffset, e.confidence, e.extractor)
    fields.map(field => field.replaceAll("\t", " ")).mkString("\t")
  }
 
  def fromString(str: String): Option[KbpExtraction] = {
    
    str.split("\t") match {
      case Array(arg1, rel, arg2, arg1postags, relpostags, arg2postags, docId, sentId, sent, sentStartOffset, sentEndOffset, confidence, extractor, _*) => {
        Some(new KbpExtraction(arg1, rel, arg2, arg1postags, relpostags, arg2postags, docId, sentId, sent, sentStartOffset, sentEndOffset, confidence, extractor))
      }
      case _ => {
        System.err.println("Error parsing KbpExtraction: %s".format(str))
        None
      }
    }
  }
  
  def fromSrlInstance(srlInst: SrlExtractionInstance): Iterable[KbpExtraction] = {
    
    val triplized = srlInst.triplize(false).filter(_.extr.arg2s.size == 1)
    throw new RuntimeException("not implemented")
  }
  
}