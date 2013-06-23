package edu.knowitall.tac2013.prep

import org.scalatest._

class ParsedKbpSentenceSpec extends FlatSpec {

  val samplesDir = "src/main/resources/samples/"
  val corpora = Seq("news", "web", "forum")
  val parsedFiles = corpora.map { c => "%s%s%s".format(samplesDir, c, "-parsed.txt") }
  val rawFiles = corpora.map { c => "%s%s%s".format(samplesDir, c, "-xml.txt") }
  
  "ParsedKbpSentences" should "deserialize then serialize to their original string" in {
    
    val testLines = parsedFiles map scala.io.Source.fromFile flatMap { _.getLines }
    testLines.foreach { line =>
      val sent = ParsedKbpSentence.read(line).get
      val reserialized = ParsedKbpSentence.write(sent)
      require(reserialized.equals(line))
    } 
  }
}