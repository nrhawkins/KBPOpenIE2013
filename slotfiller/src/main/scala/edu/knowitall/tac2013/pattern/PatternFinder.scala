package edu.knowitall.tac2013.pattern

import jp.sf.amateras.solr.scala._
import edu.knowitall.tac2013.openie.KbpExtraction

case class Pattern private (val freq: Int, val relStemmed: String, val query: KbQuery, val sampleFills: StringCounter, val sampleEntities: StringCounter) {
  
  def arg1 = query.arg1Type
  def arg2 = query.arg2Type
  
  def groupFields = Seq(arg1, relStemmed, arg2)
  def groupKey = groupFields.mkString(",")
  
  def combineWith(other: Pattern): Pattern = {
    require(this.groupKey == other.groupKey) 
    Pattern(this.freq + other.freq, relStemmed, query, this.sampleFills.addAll(other.sampleFills).trim(200), this.sampleEntities.addAll(other.sampleEntities).trim(200))
  }
  
  override def toString: String = {
    val sampleArg1s = if (query.entityArg1) sampleEntities.top(4) else sampleFills.top(4)
    val sampleArg2s = if (query.entityArg2) sampleEntities.top(4) else sampleFills.top(4)
    
    val a1String = sampleArg1s.map(p => "%s(%d)".format(p._1, p._2)).mkString(", ")
    val a2String = sampleArg2s.map(p => "%s(%d)".format(p._1, p._2)).mkString(", ")
    
    val fields = Seq(freq.toString) ++ groupFields ++ Seq(a1String, a2String)
    fields.mkString("\t")
  }
}
object Pattern {
  
  import edu.knowitall.tool.stem.MorphaStemmer
  import edu.knowitall.tool.chunk.ChunkedToken
  
  val morphaLocal = new ThreadLocal[MorphaStemmer]() {
    override def initialValue = new MorphaStemmer
  }
  def morpha = morphaLocal.get
  
  def from(freq: Int, relStemmed: String, query: KbQuery, samples: Seq[KbpExtraction]): Pattern = {
    def sampleFills = if (query.entityArg1) samples.map(_.arg2.originalText) else samples.map(_.arg1.originalText)
    def sampleEntities = if (query.entityArg1) samples.map(_.arg1.originalText) else samples.map(_.arg2.originalText)
    
    Pattern(freq, relStemmed, query, StringCounter.fromStrings(sampleFills), StringCounter.fromStrings(sampleEntities))
  }
  
  def stemRel(tokens: Seq[ChunkedToken]): String = tokens.map({ t => morpha.lemmatizeToken(t) }).map(_.lemma).mkString(" ")
}

class PatternFinder(val solrClient: SolrClient, elements: Iterable[KbElement]) {
  
  def this(url: String, elements: Iterable[KbElement]) = this(new SolrClient(url), elements)

  def sendQuery(query: KbQuery) = {
    val solrQuery = solrClient.query(query.queryString)
    val result = solrQuery.sortBy("confidence",Order.desc).rows(10000).getResultAsMap() 
    val kbpExtrs = result.documents.flatMap { doc =>
      val fieldMap = doc.asInstanceOf[Map[String, Any]]
      KbpExtraction.fromFieldMap(fieldMap)
    }
    (query, kbpExtrs)
  }
  
  def sendQueries(kbElement: KbElement): Iterable[(KbQuery, Seq[KbpExtraction])] = {
    
    // send query with arg1=entity, arg2=fill, and vice versa
    val q1 = KbQuery(kbElement, true)
    val q2 = KbQuery(kbElement, false)
    Seq(sendQuery(q1), sendQuery(q2))
  }
  
  /**
   * Return patterns grouped by fill type
   */
  def getPatterns: Map[String, Seq[Pattern]] = {
    
    System.err.println("Issuing Queries...")

    val results = elements flatMap sendQueries
    
    // flatMap results into patterns, then group patterns and combine.
    val rawPatterns = results.flatMap { case (query, extrs) =>
      val relGroups = extrs.groupBy(e => Pattern.stemRel(e.rel.tokens))
      relGroups.iterator.map { case (relStemmed, extrs) =>
        Pattern.from(extrs.size, relStemmed, query, extrs)  
      }
    }
    
    System.err.println("Combining patterns...")
    
    def combine(p1: Pattern, p2: Pattern) = p1.combineWith(p2)
    
    // combine intermediate results to reduce memory footprint...
    val intermediate = rawPatterns.grouped(10000).flatMap { group =>
      group.groupBy(_.groupKey).values.map { patterns => patterns.reduce(combine) }
    } toSeq
    
    // group patterns by key and combine
    val combinedPatterns = intermediate.groupBy(_.groupKey).values.map { patterns => patterns.reduce(combine) }
    
    System.err.println("Grouping patterns...")
    val groupedPatterns = combinedPatterns.groupBy(_.query.element.slotname)
    
    // sort patterns in descending order by frequency
    val sortedPatterns = groupedPatterns.map { case (slotname, patterns) => (slotname, patterns.toSeq.sortBy(-_.freq)) }
    
    sortedPatterns
  }
}

object PatternFinder extends App {
  
  import scopt.OptionParser
  import java.io.File
  import java.io.PrintStream
  
  var solrUrl = "http://knowitall:knowit!@rv-n16.cs.washington.edu:8123/solr"
  var queriesFile: File = new File(".") 
  var answerFile: File = new File(".")
  var output: PrintStream = System.out
  var elementLimit = Int.MaxValue
  
  val parser = new OptionParser() {
    arg("queriesFile", "Path to queries xml file", { s => queriesFile = new File(s) })
    arg("answerFile", "Path to annotations tab file", { s => answerFile = new File(s) })
    opt("output", "Output file (default stdout)", { s => output = new PrintStream(new File(s)) })
    opt("limit", "debug limit elements, default no limit", { s => elementLimit = s.toInt })
  }
  
  if (parser.parse(args)) {
    
    val elements = new AnswerKeyReader(answerFile, queriesFile)
    
    val patternFinder = new PatternFinder(solrUrl, elements.take(elementLimit))
    
    patternFinder.getPatterns.iterator.toSeq.sortBy(-_._2.size) foreach { case (slotname, patterns) =>
      output.println("PATTERNS FOR: " + slotname)
      patterns foreach output.println
      output.println
    }
  }

  if (output != System.out) output.close()
}


object KbPatternFinder extends App {
  
  import scopt.OptionParser
  import java.io.File
  import java.io.PrintStream
  
  var solrUrl = "http://knowitall:knowit!@rv-n16.cs.washington.edu:8123/solr"
  var kbPath: File = new File(".") 
  var output: PrintStream = System.out
  var elementLimit = Int.MaxValue
  
  val parser = new OptionParser() {
    arg("kbPath", "Path to knowledge base", { s => kbPath = new File(s) })
    opt("output", "Output file (default stdout)", { s => output = new PrintStream(new File(s)) })
    opt("limit", "debug limit elements, default no limit", { s => elementLimit = s.toInt })
  }
  
  if (parser.parse(args)) {
    
    val elements = new KnowledgeBaseReader(kbPath)
    
    val patternFinder = new PatternFinder(solrUrl, elements.take(elementLimit))
    
    patternFinder.getPatterns.iterator.toSeq.sortBy(-_._2.size) foreach { case (slotname, patterns) =>
      output.println("PATTERNS FOR: " + slotname)
      patterns foreach output.println
      output.println
    }
  }

  if (output != System.out) output.close()
}
