package edu.knowitall.tac2013.prep

/**
 * Represents the lines of text in a single 
 * KBP corpus document -
 * e.g. from <DOC> to </DOC>
 */
class KbpRawDoc(val lines: List[KbpDocLine])

/**
 * Represents a line of characters from the raw KBP source corpus,
 * with byte offsets counting from 0 at the start of the "<DOC>" tag.
 */
case class KbpDocLine(val line: String, val startByte: Int, val endByte: Int) {
  lazy val isBlank = line.trim.isEmpty()
  def debugString = "(%04d,%04d) %s".format(startByte, endByte, line)
  def length = endByte - startByte + 1
}

class KbpParsedDoc(
    val docIdLine: KbpDocLine, 
    val authorLine: Option[KbpDocLine], 
    val datetimeLine: Option[KbpDocLine], 
    val textLines: List[KbpDocLine]) {
  
  def debugText = {
    
    val allFields = Seq(docIdLine) ++ authorLine ++ datetimeLine ++ textLines
    allFields.map(_.debugString).mkString
  }
}