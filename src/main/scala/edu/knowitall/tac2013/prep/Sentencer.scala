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
    
    val docId = extractDocId(parsedDoc.docIdLine)
    val author = parsedDoc.authorLine flatMap extractAuthor
    val date = parsedDoc.datetimeLine flatMap extractDate
    
    if (docId.isEmpty) {
      val msgFmt = "Sentencer error #%d: Doc skipped; Unable to extract docId from line: %s"
      val msg = msgFmt.format(errorCounter.incrementAndGet(), parsedDoc.docIdLine.line)
      System.err.println(msg)
      Seq.empty
    } else {
      buildKbpSentences(docId.get, author, date, parsedDoc.textLines)
    }
  }
  

  
  /*
   * Extract docId string, assuming kbpLine contains it.
   */
  private def extractDocId(kbpLine: KbpDocLine): Option[String] = {
    // Format is either:
    // <DOCID>id here...</DOCID>		(web)
    // <DOC id="AFP_ENG_20090531.0001" type="story" >	(news)
    // <doc id="bolt-eng-DF-183-195681-7948494">	(forum)
    val str = kbpLine.line
    
    if (str.startsWith("<DOCID>")) {
      // drop the tag and go until the closing tag.
      Some(str.drop(7).takeWhile(_ != '<'))
    }
    else if (docIdPattern.matcher(str).find()) {
      // drop the <DOC ID=" part, and take until the closing quote.
      Some(str.drop(9).takeWhile(_ != '\"'))
    } else {
      // convertToSentences reports the error for us...
      None
    }
  }
  
  private val trailingWs = Pattern.compile("\\s+$")
  private val quotes = Pattern.compile("^\"([^\"]+)\"$")
  
  /*
   * Fabricate a KbpLine where any author mention is inside an extractp-able
   * sentence, where the sentence's offsets are set such that the author
   * mention falls in the correct location.
   * Returns none if an author couldn't be found within the line.
   */
  private def extractAuthor(kbpLine: KbpDocLine): Option[KbpDocLine] = {
    // Author format is always either:
    // <POSTER> "jht...@gmail.com" &lt;jht...@gmail.com&gt; </POSTER>   (web)
    // (nothing for news)
    // (nothing for forums, for now, since each doc lists many authors)
    val str = kbpLine.line

    // Pull the author's name out of the line with start and end offsets.
    val mention: Option[Mention] =
      if (str.startsWith("<POSTER>")) {
        // drop the tag, plus a space.
        // then take until the next ampersand
        val (text, quoted) = {
          val c1 = str.drop(9).takeWhile(c => c != '&' && c != '<' && c != '\n')
          val c2 = trailingWs.matcher(c1).replaceAll("")
          val matcher = quotes.matcher(c2)
          val quoted = matcher.find()
          val c3 = if (quoted) c2.drop(1).dropRight(1) else c2 
          (c3, quoted)
        }
        val startByte = if (quoted) 10 else 9
        val endByte = startByte + text.length
        Some(Mention(text, startByte, endByte, kbpLine))
      } else {
        None
      }

    mention match {
      case Some(mention) => fabricateSentence(mention, "This post was written by ", ".")
      case None => None
    }
  }

  // Fabricate a sentence containing the author's name where
  // Byte offsets still properly point to the name 
  private def fabricateSentence(m: Mention, prefix: String, suffix: String): Option[KbpDocLine] = {

    // compute start bytes for the fabricated sentence
    val fabStart = m.startByte - prefix.length + m.kbpLine.startByte
    val fabEnd = m.endByte + suffix.length + m.kbpLine.startByte
    // if fabricated offsets point to bytes outside the document (e.g. negative)
    // then we can't hand this to the caller, they'll hit an OOB exception.
    if (fabStart < 0 || fabEnd > m.kbpLine.endByte) None
    else {
      val fabSentence = Seq(prefix, m.text, suffix).mkString
      Some(new KbpDocLine(fabSentence, fabStart, fabEnd))
    }
  } 
  
  private def extractDate(kbpLine: KbpDocLine): Option[KbpDocLine] = {
    // Date format is always:
    // <DATETIME> 2007-10-22T10:31:03 </DATETIME> (web)
    // On its own line with no tags, usually prefixed by a location
    // Indicated many times per forum document
    val str = kbpLine.line
    
    val mention = {
      if (str.startsWith("<DATETIME>")) {
      val text = str.drop(11).takeWhile(_ != '<')
      Mention(text, 11, 11+text.length, kbpLine)
    } else {
      Mention(str, 0, str.length, kbpLine)
      }
    }
    
    fabricateSentence(mention, "This post was written on ", ".")
  }
  
  private val newLine = Pattern.compile("\n")
  
  private def buildKbpSentences(docId: String, author: Option[KbpDocLine], date: Option[KbpDocLine], textLines: Seq[KbpDocLine]): Seq[KbpSentence] = {
    
    // first, return any author or date lines as sentences
    val optionals = (author.toSeq ++ date.toSeq)
    
    // split textLines into groups (rough paragraphs?) around empty lines (double-newlines)
    
    val lineIterator = textLines.iterator
    val lineGroups = Iterator.continually {
      lineIterator.dropWhile(_.isBlank).takeWhile(!_.isBlank).toSeq
    } takeWhile(!_.isEmpty) toSeq
   
    // join lines of a paragraph into big strings, and stuff into a KbpDocLine
    val paragraphs = lineGroups map { lineGroup =>
      // join into one big KbpDocLine 
      // assume lines are in document order
      // replace newlines (again, assuming they are 1 byte) and replace with space char.
      val bytes = lineGroup.flatMap(l => l.line.getBytes()).toArray
      val text = new String(bytes, "UTF8")
      val start = lineGroup.head.startByte
      val end = lineGroup.last.endByte
      new KbpDocLine(text, start, end)
    }
    
    // run segmenter on each paragraph,
    // map resulting segments to the KbpDocLine structure.
    // flatten over all paragraphs
    val allSegments = paragraphs flatMap { pgraph =>
      val segments = try {
        segmenter.segment(pgraph.line)
      } catch {
        case e: Throwable => {
          System.err.println("Sentencer error #%d: Segmenter exception on input: %s".format(errorCounter.incrementAndGet(), pgraph.line))
          e.printStackTrace()
          Iterable.empty
        }
      }

      // placeholder segment is null because the second list is always shorter
      val bytes = pgraph.line.getBytes
      segments.zipAll(
          segments.drop(1).map(_.interval.start), 
          null, 
          pgraph.endByte - pgraph.startByte
          ) map { case (seg, nextStart) =>
        
        // For some reason we seem to drift behind, as if the sentencer counts offsets differently 
        // than based on the source bytes. So, here's a hack to shift our sentence window over
        // so that it starts with a Letter.
        val shift = 0 //math.max(0, bytes.drop(seg.offset).indexWhere(_.toChar.isLetter))
        val start = seg.offset + shift 
        val end = nextStart - 1 + shift
        val text = new String(bytes.drop(start).take(end - start + 1), "UTF8")
        new KbpDocLine(text, pgraph.startByte + start, pgraph.startByte + end)
      }
    }
    
    // convert KbpDocLines to KbpSentences.
    (optionals ++ allSegments).zipWithIndex map { case (kbpLine, sentNum) =>
      new KbpSentence(docId, sentNum, kbpLine.startByte, kbpLine.endByte, newLine.matcher(kbpLine.line).replaceAll(" "))
    }
  }
}

object Sentencer {
  
  import scopt.OptionParser
  
  import edu.knowitall.tool.sentence.BreezeSentencer
  
  private val docIdPattern = Pattern.compile("^<DOC\\s+id=.*", Pattern.CASE_INSENSITIVE)
  
  def main(args: Array[String]): Unit = {
    
    var inputFile = args(0)
    var corpus = args(1)
    var news = corpus.equals("news")
    var forum = corpus.equals("forum")
    var web = corpus.equals("web")
    if (!news && !forum && !web) throw new IllegalArgumentException("Unknown corpus: %s".format(args(1)))

    val docSplitter = new DocSplitter()
    val docParser = KbpDocParser.getParser(corpus)
    val sentencer = new Sentencer(new BreezeSentencer())
    
    val source = io.Source.fromFile(inputFile)
    
    val docs = docSplitter.splitDocs(source)
    val parsedDocs = docs flatMap docParser.parseDoc
    val sentences = parsedDocs foreach { doc =>
      sentencer.convertToSentences(doc) foreach { s =>
        println(KbpSentence.write(s))
      }
    }
  }
}

/*
 * Represents a specific subsection of a given KbpLine. 
 * 
 */
case class Mention(val text: String, val startByte: Int, val endByte: Int, val kbpLine: KbpDocLine)


