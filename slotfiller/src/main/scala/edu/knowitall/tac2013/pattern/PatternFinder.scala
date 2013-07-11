package edu.knowitall.tac2013.pattern

import jp.sf.amateras.solr.scala._
import edu.knowitall.tac2013.openie.KbpExtraction

case class Pattern(val freq: Int, val relString: String, val query: KbQuery, val sampleFills: StringCounter, val sampleEntities: StringCounter) {
  
  def arg1 = query.arg1Type
  def arg2 = query.arg2Type
  
  def groupFields = Seq(arg1, relString, arg2)
  def groupKey = groupFields.mkString(",")
  
  def combineWith(other: Pattern): Pattern = {
    require(this.groupKey == other.groupKey) 
    Pattern(this.freq + other.freq, relString, query, this.sampleFills.addAll(other.sampleFills).trim(100), this.sampleEntities.addAll(other.sampleEntities).trim(100))
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
  
  def from(freq: Int, relString: String, query: KbQuery, samples: Seq[KbpExtraction]): Pattern = {
    def sampleFills = if (query.entityArg1) samples.map(_.arg2.originalText) else samples.map(_.arg1.originalText)
    def sampleEntities = if (query.entityArg1) samples.map(_.arg1.originalText) else samples.map(_.arg2.originalText)
    
    Pattern(freq, relString, query, StringCounter.fromStrings(sampleFills), StringCounter.fromStrings(sampleEntities))
  }
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
  
  def getPatterns: Seq[Pattern] = {
    
    System.err.println("Issuing Queries...")
    
    val results = elements flatMap sendQueries
    
    // flatMap results into patterns, then group patterns and combine.
    val rawPatterns = results.flatMap { case (query, extrs) =>
      val relGroups = extrs.groupBy(_.rel.originalText)
      relGroups.iterator.map { case (relString, extrs) =>
        Pattern.from(extrs.size, relString, query, extrs)  
      }
    }
    
    System.err.println("Grouping patterns...")
    
    // group patterns by key and combine
    def combine(p1: Pattern, p2: Pattern) = p1.combineWith(p2)
    val combinedPatterns = rawPatterns.groupBy(_.groupKey).values.map { patterns => patterns.reduce(combine) }
    
    System.err.println("Sorting patterns...")
    
    // sort patterns in descending order by frequency
    val sortedPatterns = combinedPatterns.toSeq.sortBy(-_.freq)
    
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
  
  val parser = new OptionParser() {
    arg("queriesFile", "Path to queries xml file", { s => queriesFile = new File(s) })
    arg("answerFile", "Path to annotations tab file", { s => answerFile = new File(s) })
    opt("output", "Output file (default stdout)", { s => output = new PrintStream(new File(s)) })
  }
  
  if (parser.parse(args)) {
    
    val elements = new AnswerKeyReader(answerFile, queriesFile)
    
    val patternFinder = new PatternFinder(solrUrl, elements)
    
    patternFinder.getPatterns foreach println
  }
}
