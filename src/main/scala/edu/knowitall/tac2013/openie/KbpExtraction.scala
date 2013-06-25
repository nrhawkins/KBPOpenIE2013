package edu.knowitall.tac2013.openie

import edu.knowitall.srlie.SrlExtractionInstance
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.tool.parse.graph.DependencyGraph
import edu.knowitall.chunkedextractor.BinaryExtractionInstance
import edu.knowitall.chunkedextractor.Relnoun
import edu.knowitall.srlie.confidence.SrlConfidenceFunction.SrlConfidenceFunction
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.chunkedextractor.ExtractionPart
import edu.knowitall.tac2013.prep.ParsedKbpSentence
import edu.knowitall.srlie.SrlExtraction

abstract class KbpRelation {
  def tokenInterval: Interval
  def originalText: String
  def tokens: Seq[PostaggedToken]
}
object KbpRelation {
  def fromSrlRelation(rel: SrlExtraction.Relation) = new KbpRelation() {
    val tokenInterval = rel.span
    val originalText = rel.text
    val tokens = rel.tokens
  }
  def fromRelnounRelation(rel: ExtractionPart[Relnoun.Token]) = new KbpRelation() {
    val tokenInterval = rel.interval
    val originalText = rel.text
    val tokens = rel.tokens
  }
}

abstract class KbpArgument {
  def tokenInterval: Interval
  def originalText: String
  def tokens: Seq[PostaggedToken]
  def wikiLink: Option[String]
  def types: Seq[String] // Seq[Type]?
}
object KbpArgument {
  
  val intervalRegex = "%d %d".r
  
  def fromSrlArgument(arg: SrlExtraction.Argument) = new KbpArgument() {
    val tokenInterval = arg.tokenInterval
    val originalText = arg.text
    val tokens = arg.tokens
    val wikiLink = None
    val types = Nil
  }
  def fromRelnounArgument(arg: ExtractionPart[Relnoun.Token]) = new KbpArgument() {
    val tokenInterval = arg.interval
    val originalText = arg.text
    val tokens = arg.tokens
    val wikiLink = None
    val types = Nil
  }
  
  // Just write the interval and original text.
  // Tokens are reconstructed from the sentence.
  def write(arg: KbpArgument): String = {
    val intervalString = "%d %d".format(arg.tokenInterval.start, arg.tokenInterval.end)
    val wikiLinkString = arg.wikiLink.getOrElse("")
    val typeString = arg.types.mkString(" ")
    Seq(intervalString, arg.originalText, wikiLinkString, typeString).map(_.replaceAll("\t", " ")).mkString(" ")
  }
}

case class KbpExtraction(
  val arg1: KbpArgument,
  val rel: KbpRelation,
  val arg2: KbpArgument,
  val confidence: Double,
  val extractor: String,
  val sentence: ParsedKbpSentence
)

object KbpExtraction {
  
  def write(extr: KbpExtraction): String = throw new RuntimeException("not implemented")
  
  def fromRelnounInstance(
      relnounInst: BinaryExtractionInstance[Relnoun.Token], 
      parsedSentence: ParsedKbpSentence): KbpExtraction = {
    
    val extr = relnounInst.extr
    
    new KbpExtraction(
        arg1 = KbpArgument.fromRelnounArgument(extr.arg1),
        rel = KbpRelation.fromRelnounRelation(extr.rel),
        arg2 = KbpArgument.fromRelnounArgument(extr.arg1),
        confidence = 0.9, 
        extractor = "relnoun",
        sentence = parsedSentence)
  }
  
  def fromSrlInstance(
      srlInst: SrlExtractionInstance, 
      parsedSent: ParsedKbpSentence, 
      confFunc: SrlConfidenceFunction): Iterable[KbpExtraction] = {
    
    val triplized = srlInst.triplize(false).filter(_.extr.arg2s.size == 1) // Some could have zero arg2s
    
    val kbpExtractions = triplized.map { triple =>
      val extr = triple.extr
      new KbpExtraction(
        arg1 = KbpArgument.fromSrlArgument(extr.arg1),
        rel = KbpRelation.fromSrlRelation(extr.rel),
        arg2 = KbpArgument.fromSrlArgument(extr.arg2s(0)), // requires triplized filter { args.size == 1 } 
        confidence = confFunc.getConf(triple),
        extractor = "srl",
        sentence = parsedSent
      )
    }
    kbpExtractions
  }
}