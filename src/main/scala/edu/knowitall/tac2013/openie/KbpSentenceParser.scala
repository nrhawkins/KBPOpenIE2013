package edu.knowitall.tac2013.openie

import edu.knowitall.tool.chunk.Chunker
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.parse.DependencyParser
import edu.knowitall.tool.postag.OpenNlpPostagger
import edu.knowitall.tool.tokenize.OpenNlpTokenizer
import edu.knowitall.tool.parse.ClearParser
import edu.knowitall.tool.postag.ClearPostagger
import scopt.OptionParser
import scala.io.Source
import java.io.PrintStream
import edu.knowitall.common.Resource.using
import edu.knowitall.common.Timing
import edu.knowitall.collection.immutable.Interval

import edu.knowitall.tool.chunk.ChunkedToken

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
    val chunked = chunker.synchronized { chunker.chunk(kbpSentence.text) } 
    val dgraph = parser.dependencyGraph(kbpSentence.text).serialize
    val tokens = chunked.map(_.string).mkString(" ")
    val postags = chunked.map(_.postag).mkString(" ")
    val chunks = chunked.map(_.chunk).mkString(" ")
    val offsets = chunked.map(_.offset).mkString(" ")
    
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
    
    var inputFile = Option.empty[String]
    var outputFile = Option.empty[String]
    var parallel = false
    
    val cliParser = new OptionParser() {
      opt("inputFile", "File: KbpSentences one per line -- default stdin", { str => inputFile = Some(str) })
      opt("outputFile", "File: ParsedKbpSentences, default stdout", { str => outputFile = Some(str) })
      opt("parallel", "Run multithreaded? Default = no", { parallel = true })
    }

    val nsTime = Timing.time {

      if (cliParser.parse(args)) {
        val inputSource = inputFile map Source.fromFile getOrElse Source.stdin
        val outputStream = outputFile map { file => new PrintStream(file) } getOrElse System.out
        using(inputSource) { input =>
          using(outputStream) { output =>
            if (!parallel) run(input, output)
            else runParallel(input, output)
          }
        }
      } else {
        return
      }
    }
    
    val seconds = Timing.Seconds.format(nsTime)
    System.err.println("Processed %d sentences in %s.".format(sentencesProcessed.get, seconds))
  }
  
  def run(input: Source, output: PrintStream): Unit = {

    
    val kbpProcessor = new KbpSentenceParser();
    
    val kbpSentences = input.getLines.flatMap { line =>
      sentencesProcessed.incrementAndGet()
      KbpSentence.read(line) 
    }
    
    kbpSentences.foreach { kbpSentence =>
      val parsedKbpSentenceOption = kbpProcessor.parseKbpSentence(kbpSentence)
      parsedKbpSentenceOption foreach { parsedKbpSentence =>
        output.println(ParsedKbpSentence.write(parsedKbpSentence))
      }
    }
  }
  
  val batchSize = 1000
  
  def runParallel(input: Source, output: PrintStream): Unit = {
    val chunker = new OpenNlpChunker()
    val parser = new ClearParser(new ClearPostagger())
    
    val kbpProcessor = new KbpSentenceParser();
    
    val lineGroups = input.getLines.grouped(batchSize)
    
    lineGroups.flatMap { group =>
      group.par.flatMap { line =>
        sentencesProcessed.incrementAndGet()
        KbpSentence.read(line) flatMap kbpProcessor.parseKbpSentence
      }
    } map { parsed => 
      ParsedKbpSentence.write(parsed)  
    } foreach { string =>
      output.println(string)
    }
  }
}