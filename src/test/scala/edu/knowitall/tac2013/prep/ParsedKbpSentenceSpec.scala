package edu.knowitall.tac2013.prep

import org.scalatest._
import edu.knowitall.tool.parse.graph.DependencyGraph

class ParsedKbpSentenceSpec extends FlatSpec {  
  
  val samplesDir = "src/main/resources/samples/"
  val corpora = Seq("news", "web", "forum")
  val parsedFiles = corpora.map { c => "%s%s%s".format(samplesDir, c, "-parsed.txt") }
  val rawFiles = corpora.map { c => "%s%s%s".format(samplesDir, c, "-xml.txt") }
  
  
  
  val docSplitter = new DocSplitter()
  
  "ParsedKbpSentences" should "deserialize then serialize to their original string" in {
    
    val testLines = parsedFiles map { f => io.Source.fromFile(f, "UTF8") } flatMap { _.getLines }
    testLines.foreach { line =>
      val sent = ParsedKbpSentence.read(line).get
      val reserialized = ParsedKbpSentence.write(sent)
      require(reserialized.equals(line))
    } 
  }
  
  "ParsedKbpSentences" should "contain token offsets that correctly match file contents" in {
    
    val corpusFiles = corpora.zip(rawFiles)
    
    // Load ParsedKbpSentences into a map by docId
    val parsedSentences = {
      val flat = parsedFiles map { f => io.Source.fromFile(f, "UTF8") } flatMap { _.getLines } flatMap { line => ParsedKbpSentence.read(line) }
      flat.groupBy(_.docId)
    } 
    
    // Load Docs into an iterator or list
    val corpusDocs = corpusFiles.map { case (corpus, file) =>
      (corpus, docSplitter.splitDocs(io.Source.fromFile(file, "UTF8")))
    }
    
    val corpusParsers = corpora.map(c => (c, KbpDocParser.getParser(c))).toMap

    // load ParsedDocs into a map, with source RawDocs, by DocID
    val corpusParsedDocs = corpusDocs flatMap {
      case (corpus, docs) =>
        val parser = corpusParsers(corpus)
        docs flatMap { doc =>
          parser.parseDoc(doc) flatMap { parsed =>
            parsed.extractDocId map { d => (d, (doc, parsed)) }
          }
        }
    } toMap
    
    for (
        docId <- corpusParsedDocs.keys;
        (rawDoc, parsedDoc) <- corpusParsedDocs.get(docId);
        sentenceopt <- parsedSentences.get(docId)) {
      
      val docString = rawDoc.getString
      for (sentence <- sentenceopt; token <- sentence.chunkedTokens) {
        val pstart = sentence.offset.toInt
        val start = token.interval.start + pstart
        val str = docString.drop(start).take(token.string.length).replaceAll("\n", " ")
        
        if (!str.equals(token.string)) {
          System.err.println("%s\n\"%s\"\n\"%s\"".format(sentence.docId, str, token.string))
          fail()
        }
      }
    }
  }
}