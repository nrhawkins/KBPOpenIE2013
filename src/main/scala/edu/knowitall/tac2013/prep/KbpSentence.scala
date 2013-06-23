package edu.knowitall.tac2013.prep

case class KbpSentence(val docId: String, val sentNum: Int, val offset: Int, val text: String) {
  def length = text.length
}

object KbpSentence {
  
val tabRegex = "\t".r
  
  def read(pickle: String): Option[KbpSentence] = {
    tabRegex.split(pickle) match {
      case Array(docId, sentNum, offset, text, _*) => Some(KbpSentence(docId, sentNum.toInt, offset.toInt, text))
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