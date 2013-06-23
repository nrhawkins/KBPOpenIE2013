package edu.knowitall.tac2013.prep

import org.scalatest._

class KbpSentenceSpec extends FlatSpec {

  val samplesDir = "src/main/resources/samples/"
  val corpora = Seq("news", "web", "forum")
  val sentFiles = corpora.map { c => "%s%s%s".format(samplesDir, c, "-sentences.txt") }
  val rawFiles = corpora.map { c => "%s%s%s".format(samplesDir, c, "_sample.txt") }
  
  "KbpSentences" should "deserialize then serialize to their original string" in {
    
    val testSrcs = sentFiles map scala.io.Source.fromFile
    val lines = testSrcs.flatMap(_.getLines)
    lines.foreach { line =>
      val sent = KbpSentence.read(line).get
      val reserialized = KbpSentence.write(sent)
      require(reserialized.equals(line))
    } 
  }
  
  "KbpSentences" should "have offsets that correctly key into source doc" in {
    
    val docSplitter = new DocSplitter()
    
    val sentenceMap = {
      val testSrcs = sentFiles map scala.io.Source.fromFile
      val lines = testSrcs.flatMap(_.getLines)
      val sents = lines.flatMap { s => KbpSentence.read(s) } filter { !_.text.startsWith("This post was written") }
      sents groupBy { s => s.docId }
    }

    corpora.zip(rawFiles) foreach {
      case (corpus, sample) => 
        val source = io.Source.fromFile(sample)
        val parser = KbpDocParser.getParser(corpus)
        val docSplitterator = docSplitter.splitDocs(source)
        for (rawDoc <- docSplitterator; parsedDoc <- parser.parseDoc(rawDoc); docId <- parsedDoc.extractDocId) {
          val rawBytes = rawDoc.getBytes
          val sents = sentenceMap(docId)
          sents foreach { s =>
            val bytes = rawBytes.drop(s.startByte).take(s.endByte - s.startByte + 1)
            val extrString = new String(bytes, "UTF8").replaceAll("\n", " ")
            if (!extrString.equals(s.text)) {
              System.err.println("\"%s\"".format(extrString))
              System.err.println("\"%s\"".format(s.text))
              fail()
            }
          }
        }
    }
  }
}