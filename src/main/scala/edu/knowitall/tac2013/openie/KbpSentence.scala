package edu.knowitall.tac2013.openie

case class KbpSentence(val docId: String, val sentId: Int, val text: String)

object KbpSentence {
  
val tabRegex = "\t".r
  
  def read(pickle: String): Option[KbpSentence] = {
    tabRegex.split(pickle) match {
      case Array(docId, sentIdString, text, _*) => Some(KbpSentence(docId, sentIdString.toInt, text))
      case _ => {
        System.err.println("Error reading KbpSentence: %s".format(pickle))
        None
      }
    }
  }
  
  def write(sent: KbpSentence): String = {
    val fields = KbpSentence.unapply(sent).get.productIterator.map(_.toString).toSeq
    fields.map(_.replaceAll("\t", "")).mkString("\t")
  }
}