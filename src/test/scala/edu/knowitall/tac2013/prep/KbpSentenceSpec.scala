package edu.knowitall.tac2013.prep

import org.scalatest._

class KbpSentenceSpec extends FlatSpec {

  val samplesDir = "src/main/resources/samples/"
  val files = Seq("news", "web", "forum").map { c => "%s%s%s".format(samplesDir, c, "-sentences.txt") }
  
  "KbpSentences" should "deserialize then serialize to their original string" in {
    
    val testSrcs = files map scala.io.Source.fromFile
    val lines = testSrcs.flatMap(_.getLines)
    lines.foreach { line =>
      val sent = KbpSentence.read(line).get
      val reserialized = KbpSentence.write(sent)
      require(reserialized.equals(line))
    } 
  }
}