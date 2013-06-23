package edu.knowitall.tac2013.prep

import org.scalatest._
import edu.knowitall.tool.sentence.BreezeSentencer
import java.io.File

class SentencerSpec extends FlatSpec {
  
  val docSplitter = new DocSplitter()
  val sentencer = new Sentencer(new BreezeSentencer())
  // tuple of (Parser for corpus, corpus sample/test file)
  // can add to this list to add new test sample files. 
  val corpora = Seq(
    (KbpDocParser.getParser("web"),  "src/main/resources/samples/docs-split/web"),
    (KbpDocParser.getParser("news"), "src/main/resources/samples/docs-split/news"),
    (KbpDocParser.getParser("forum"),"src/main/resources/samples/docs-split/forum")
  )

  // Except for newlines being converted to spaces, should match exactly.
  "The Sentencer" should "produce meaningful byte offsets" in {

    corpora foreach {
      case (docParser, sampleDir) => {
        for (
            file <- new File(sampleDir).listFiles;
            rawDoc <- docSplitter.splitDocs(io.Source.fromFile(file, "UTF8"));
            parsedDoc <- docParser.parseDoc(rawDoc).toList;
            s <- sentencer.convertToSentences(parsedDoc)
         ) {
          val bytes = DocSplitterSpec.fileBytes(file)
          val byteString = new String(bytes.drop(s.startByte).take(s.endByte - s.startByte + 1), "UTF8")
          // skip the fabricated sentences. Offsets only line up for the entity in them.
          if (!s.text.startsWith("This post was written")) {
            
            val str = byteString.replaceAll("\n", " ")
            val exp = s.text
            
            def bytes(str: String) = str
            if (!str.equals(exp)) {
             //System.err.println("\"%s\"".format(str))
             //System.err.println("\"%s\"".format(exp))
             fail()
            } 
          }
        }
      }
    }
  }
}