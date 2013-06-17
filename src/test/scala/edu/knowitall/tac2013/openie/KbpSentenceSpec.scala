package edu.knowitall.tac2013.openie

import org.scalatest._

class ParsedKbpSentenceSpec extends FlatSpec {

  "ParsedKbpSentences" should "deserialize then serialize to their original string" in {
    
    val testSrc = scala.io.Source.fromFile("src/main/resources/samples/parsedKbpSentences.1k")
    val lines = testSrc.getLines
    lines.foreach { line =>
      val sent = ParsedKbpSentence.read(line).get
      val reserialized = ParsedKbpSentence.write(sent)
      require(reserialized.equals(line))
    } 
  }
}