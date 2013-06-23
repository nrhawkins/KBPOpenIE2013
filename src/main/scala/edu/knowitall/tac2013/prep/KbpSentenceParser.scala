package edu.knowitall.tac2013.prep
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.postag.OpenNlpPostagger
import edu.knowitall.tool.tokenize.OpenNlpTokenizer
import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.postag.ClearPostagger
import scopt.OptionParser
import scala.io.Source
import java.io.PrintStream
import edu.knowitall.common.Resource.using
import edu.knowitall.common.Timing
import java.util.concurrent.atomic.AtomicInteger
import scala.Option.option2Iterable

class KbpSentenceParser() {

  val chunkerModel = OpenNlpChunker.loadDefaultModel
  val postagModel = OpenNlpPostagger.loadDefaultModel
  val tokenModel = OpenNlpTokenizer.loadDefaultModel
  val chunkerLocal = new ThreadLocal[OpenNlpChunker] {
    override def initialValue = {
      val tokenizer = new OpenNlpTokenizer(tokenModel)
      val postagger = new OpenNlpPostagger(postagModel, tokenizer)
      new OpenNlpChunker(chunkerModel, postagger)
    }
  }
  
  val parser = new ClearParser(new ClearPostagger())
  
  def isValid(kbpSentence: KbpSentence): Boolean = {
    kbpSentence.text.length <= 500
  }
  
  def parseKbpSentence(kbpSentence: KbpSentence): Option[ParsedKbpSentence] = {
    
    if (!isValid(kbpSentence)) return None
    try {
    // chunks, then parse
    val chunker = chunkerLocal.get
    val chunked = chunker.chunk(kbpSentence.text) 
    val dgraph = parser.dependencyGraph(kbpSentence.text).serialize
    val tokens = chunked.map(_.string).mkString(" ")
    val postags = chunked.map(_.postag).mkString(" ")
    val chunks = chunked.map(_.chunk).mkString(" ")
    val offsets = chunked.map(_.offset + kbpSentence.startByte).mkString(" ")
    
    Some(ParsedKbpSentence(kbpSentence.docId, tokens, postags, chunks, offsets, dgraph))
    } catch {
      case e: Throwable =>
        System.err.println("Error parsing sentence: %s".format(kbpSentence.text))
        e.printStackTrace()
        None
    }
  }
}


object KbpSentenceParser {
  
  import java.util.concurrent.atomic.AtomicInteger
  
  private val sentencesProcessed = new AtomicInteger(0)
  
  def main(args: Array[String]): Unit = {
    
    var webRaw  = Option.empty[String]
    var newsRaw  = Option.empty[String]
    var forumRaw = Option.empty[String] 
    
    var outputFile = Option.empty[String]
    var parallel = false
    
    val cliParser = new OptionParser() {
      opt("webFile", "raw web xml", { str => webRaw = Some(str) })
      opt("newsRaw", "raw news xml", { str => newsRaw = Some(str) })
      opt("forumRaw", "raw forum xml", { str => forumRaw = Some(str) })
      opt("outputFile", "File: ParsedKbpSentences, default stdout", { str => outputFile = Some(str) })
      opt("parallel", "Run multithreaded? Default = no", { parallel = true })
    }

    val nsTime = Timing.time {
      if (cliParser.parse(args)) {

        val inputCorpora =
          Seq((newsRaw, "news"), (webRaw, "web"), (forumRaw, "forum"))
            .flatMap {
              case (input, corpora) => input match {
                case Some(f) => Some(f, corpora)
                case None => None
              }
            }

        runMain(inputCorpora, System.out, parallel)
      }

    }
    val seconds = Timing.Seconds.format(nsTime)
    System.err.println("Processed %d sentences in %s.".format(sentencesProcessed.get, seconds))
  }

  def runMain(inputCorpora: Seq[(String, String)], output: PrintStream, parallel: Boolean): Unit = {

    println(inputCorpora)
    
    inputCorpora map { case (sample, corpus) =>
      val source = Source.fromFile(sample)
      if (parallel) runParallel(source, output, corpus)
      else run(source, output, corpus)
    }
  }
  
  def run(input: Source, output: PrintStream, corpus: String): Unit = {

    val docSplitter = new DocSplitter()
    val docParser = KbpDocParser.getParser(corpus)
    val sentencer = Sentencer.defaultInstance
    val kbpProcessor = new KbpSentenceParser();
    
    val docs = docSplitter.splitDocs(input)
    val parsedDocs = docs flatMap docParser.parseDoc
    val sentences = parsedDocs flatMap sentencer.convertToSentences
    
    sentences.foreach { kbpSentence =>
      val parsedKbpSentenceOption = kbpProcessor.parseKbpSentence(kbpSentence)
      parsedKbpSentenceOption foreach { parsedKbpSentence =>
        output.println(ParsedKbpSentence.write(parsedKbpSentence))
      }
    }
  }
  
  val batchSize = 1000
  
  def runParallel(input: Source, output: PrintStream, corpus: String): Unit = {
    
    val docSplitter = new DocSplitter()
    val docParser = KbpDocParser.getParser(corpus)
    val sentencer = Sentencer.defaultInstance
    val kbpProcessor = new KbpSentenceParser();
    
    val docs = docSplitter.splitDocs(input)
    val parsedDocs = docs flatMap docParser.parseDoc
    val sentences = parsedDocs flatMap sentencer.convertToSentences
    
    sentences.grouped(batchSize).flatMap { sentenceGroup =>
      sentenceGroup.par.flatMap { sentence =>
        sentencesProcessed.incrementAndGet()
        kbpProcessor.parseKbpSentence(sentence)
      }
    } map { parsed => 
      ParsedKbpSentence.write(parsed)  
    } foreach { string =>
      output.println(string)
    }
  }
}