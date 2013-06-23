package edu.knowitall.tac2013.prep

import java.util.concurrent.atomic.AtomicInteger
import java.util.regex.Pattern
import edu.knowitall.tool.segment.Segmenter
import Sentencer._

/**
 * Converts from KbpParsedDoc to KbpSentences
 */
class Sentencer(val segmenter: Segmenter) {
  
  private val errorCounter = new AtomicInteger(0)
  
  /**
   * Returns an empty collection on error.
   */
  def convertToSentences(parsedDoc: KbpParsedDoc): Seq[KbpSentence] = {
    
    val docId = parsedDoc.extractDocId
    val author = parsedDoc.extractAuthor
    val date = parsedDoc.extractDate
    
    if (docId.isEmpty) {
      val msgFmt = "Sentencer error #%d: Doc skipped; Unable to extract docId from line: %s"
      val msg = msgFmt.format(errorCounter.incrementAndGet(), parsedDoc.docIdLine.line)
      System.err.println(msg)
      Seq.empty
    } else {
      buildKbpSentences(docId.get, author, date, parsedDoc.textLines)
    }
  }

  private val newLine = Pattern.compile("\n")
  
  private def buildParagraphs(lineIterator: Iterator[KbpDocLine]): Seq[KbpDocLine] = {
    val lineGroups = Iterator.continually {
      lineIterator.dropWhile(_.isBlank).takeWhile(!_.isBlank).toSeq
    } takeWhile(!_.isEmpty) toSeq
   
    // join lines of a paragraph into big strings, and stuff into a KbpDocLine
    val paragraphs = lineGroups map { lineGroup =>
      // join into one big KbpDocLine 
      // assume lines are in document order
      val bytes = lineGroup.flatMap(l => l.line.getBytes("UTF8")).toArray
      val text = new String(bytes, "UTF8")
      val start = lineGroup.head.startByte
      val end = lineGroup.last.endByte
      assert(end == start + bytes.length - 1)
      new KbpDocLine(text, start, end)
    }
    paragraphs
  }
  
  private def buildKbpSentences(docId: String, author: Option[KbpDocLine], date: Option[KbpDocLine], textLines: Seq[KbpDocLine]): Seq[KbpSentence] = {
    
    // first, return any author or date lines as sentences
    val optionals = (author.toSeq ++ date.toSeq)
    
    // split textLines into groups (rough paragraphs?) around empty lines (double-newlines)
    val paragraphs = buildParagraphs(textLines.iterator)
    
    // run segmenter on each paragraph,
    // map resulting segments to the KbpDocLine structure.
    // flatten over all paragraphs
    val allSegments = paragraphs map { pgraph =>
      val segments = try {
        segmenter.segment(pgraph.line).toSeq
      } catch {
        case e: Throwable => {
          System.err.println("Sentencer error #%d: Segmenter exception on input: %s".format(errorCounter.incrementAndGet(), pgraph.line))
          e.printStackTrace()
          Seq.empty
        }
      }
      (segments, pgraph.startByte)
    }
    
    val asDocLines = allSegments.flatMap { case (segments, pgraphStart) =>
      val segBytes = segments.map(_.text.getBytes("UTF8"))
      val segStarts = (0 to segBytes.size - 1).map { num =>
        val segOffset = segBytes.take(num).map(_.length).sum
        segOffset + pgraphStart
      }
      assert(segBytes.size == segStarts.size && segStarts.size == segments.size)
      segments.zip(segBytes).zip(segStarts) map { case ((seg, bytes), start) =>
        new KbpDocLine(new String(bytes, "UTF8"), start, start + bytes.length - 1)  
      }
    }
    
    // convert KbpDocLines to KbpSentences.
    (optionals ++ asDocLines).zipWithIndex map { case (kbpLine, sentNum) =>
      new KbpSentence(docId, sentNum, kbpLine.startByte, kbpLine.endByte, newLine.matcher(kbpLine.line).replaceAll(" "))
    }
  }
}

object Sentencer {
  
  import scopt.OptionParser
  
  import edu.knowitall.tool.sentence.BreezeSentencer
  
  lazy val defaultInstance = new Sentencer(new BreezeSentencer())
  
  def main(args: Array[String]): Unit = {
    
    var inputFile = args(0)
    var outputFile = args(1)
    var corpus = args(2)
    var news = corpus.equals("news")
    var forum = corpus.equals("forum")
    var web = corpus.equals("web")
    if (!news && !forum && !web) throw new IllegalArgumentException("Unknown corpus: %s".format(args(1)))

    val docSplitter = new DocSplitter()
    val docParser = KbpDocParser.getParser(corpus)
    val sentencer = defaultInstance
    
    val source = io.Source.fromFile(inputFile)
    val output = if (outputFile.equals("stdout")) System.out else new java.io.PrintStream(outputFile)
    
    val docs = docSplitter.splitDocs(source)
    val parsedDocs = docs flatMap docParser.parseDoc
    val sentences = parsedDocs foreach { doc =>
      sentencer.convertToSentences(doc) foreach { s =>
        output.println(KbpSentence.write(s))
      }
    }
  }
}

/*
 * Represents a specific subsection of a given KbpLine. 
 * 
 */
case class Mention(val text: String, val startByte: Int, val endByte: Int, val kbpLine: KbpDocLine)


