package edu.knowitall.tac2013.openie

import edu.knowitall.common.Resource
import edu.knowitall.common.Timing

import edu.knowitall.srlie.SrlExtractor
import edu.knowitall.srlie.SrlExtractionInstance
import edu.knowitall.srlie.confidence.SrlConfidenceFunction

import edu.knowitall.chunkedextractor.Relnoun

import edu.knowitall.tool.parse.graph.DependencyGraph
import edu.knowitall.tool.conf.impl.LogisticRegression
//import edu.knowitall.tool.chunk.OpenNlpChunker
//import edu.knowitall.tool.postag.OpenNlpPostagger
//import edu.knowitall.tool.tokenize.OpenNlpTokenizer
import edu.knowitall.tool.stem.MorphaStemmer

import java.util.concurrent.atomic.AtomicInteger
import java.io.PrintStream

import scopt.OptionParser


abstract class KbpExtractor {

  protected val errorCounter = new AtomicInteger(0)

  def extract(parsedSentence: ParsedKbpSentence): Seq[KbpExtraction]
}

object KbpExtractor {
  
  object Settings {
    var inputFile = "stdin"
    var outputFile = "stdout"
  }
  
  def main(args: Array[String]): Unit = {
    
    val parser = new OptionParser("KbpExtractor") {
      opt("inputFile", "ParsedKbpSentences for input, default stdinput", { s => Settings.inputFile = s })
      opt("outputFile", "KbpExtractionInstances output file, default stdout", { s => Settings.outputFile = s})
    }
    
    if (!parser.parse(args)) return
    
    def getInput = if (Settings.inputFile.equals("stdin")) io.Source.stdin else io.Source.fromFile(Settings.inputFile)
    def getOutput = if (Settings.outputFile.equals("stdout")) System.out else new PrintStream(Settings.outputFile)
    
    val extractor = new KbpCombinedExtractor()
    
    Resource.using(getInput) { input =>
      Resource.using(getOutput) { output =>
        val sentences = input.getLines.flatMap(str => ParsedKbpSentence.read(str))
        val insts = sentences.flatMap { sent =>
          val extrs = extractor.extract(sent)
          extrs.map(extr => new KbpExtractionInstance(extr, sent))
        }
        val outStrings = insts map KbpExtractionInstance.write
        outStrings foreach output.println
      }
    }
  }
}

class KbpCombinedExtractor(
  val kbpSrl: KbpSrlExtractor = new KbpSrlExtractor(),
  val kbpRelnoun: KbpRelnounExtractor = new KbpRelnounExtractor()) extends KbpExtractor {

  def extract(parsedSentence: ParsedKbpSentence): Seq[KbpExtraction] = {
    kbpSrl.extract(parsedSentence) ++ kbpRelnoun.extract(parsedSentence)
  }
}

class KbpRelnounExtractor(val relnoun: Relnoun = new Relnoun()) extends KbpExtractor {

//  val chunkerModel = OpenNlpChunker.loadDefaultModel
//  val postagModel = OpenNlpPostagger.loadDefaultModel
//  val tokenModel = OpenNlpTokenizer.loadDefaultModel
//
//  val chunkerLocal = new ThreadLocal[OpenNlpChunker] {
//    override def initialValue = {
//
//      val tokenizer = new OpenNlpTokenizer(tokenModel)
//      val postagger = new OpenNlpPostagger(postagModel, tokenizer)
//      new OpenNlpChunker(chunkerModel, postagger)
//    }
//  }

  override def extract(parsedSentence: ParsedKbpSentence): Seq[KbpExtraction] = {

    val chunked = parsedSentence.chunkedTokens
    val lemmatized = chunked map MorphaStemmer.lemmatizeToken

    val relnounExtractionInstances = try {
      relnoun.extract(lemmatized).toSeq
    } catch {
      case e: Throwable => {
        System.err.println(
          "Relnoun error #%d parsing input: %s".format(errorCounter.incrementAndGet(), parsedSentence.text))
        Seq.empty
      }
    }

    relnounExtractionInstances.map { inst =>
      KbpExtraction.fromRelnounInstance(inst, chunked, parsedSentence)
    }
  }
}

class KbpSrlExtractor(
  val srl: SrlExtractor = new SrlExtractor(),
  val confFunc: LogisticRegression[SrlExtractionInstance] = SrlConfidenceFunction.loadDefaultClassifier())
  extends KbpExtractor {

  override def extract(parsedSentence: ParsedKbpSentence): Seq[KbpExtraction] = {

    val graph = DependencyGraph.deserialize(parsedSentence.dgraph)

    val srlInstances =
      try {
        srl.apply(graph)
      } catch {
        case e: Throwable =>
          System.err.println(
            "SrlExtractor error #%d parsing input: %s".format(errorCounter.incrementAndGet(), parsedSentence.text))
          Seq.empty
      }

    val kbpExtractions = srlInstances.flatMap { inst =>
      KbpExtraction.fromSrlInstance(inst, parsedSentence, confFunc)
    }

    kbpExtractions
  }
}

