package edu.knowitall.tac2013.experiment

import edu.knowitall.tac2013.findSlotFillersApp.KBPSlotOpenIERelationTranslator
import edu.knowitall.tac2013.findSlotFillersApp.KbpSlotToOpenIEData
import edu.knowitall.tac2013.findSlotFillersApp.QueryBuilder
import edu.knowitall.tac2013.openie.solr.SolrSimpleExecutor

/**
 * Runs test queries for all of the per:/org: patterns and reports statistics about
 * how many results match for a given entity, or for a wildcard entity.
 */
class PatternStats(val solrExec: SolrSimpleExecutor) {
  def this(url: String) = this(new SolrSimpleExecutor(url, 1000))

  val sampleOrgs = Set("Carnival Cruise", "Microsoft", "Apple", "Washington Post", "Discovery Channel", "Stanford University")
  val samplePers = Set("Bill Gates", "Steve Jobs", "Ronnie Sinclair", "Barack Obama", "Steve Ballmer", "Abraham Lincoln", "George Washington", "Babe Ruth")
  
  def run() {

    val orgPatterns = KBPSlotOpenIERelationTranslator.getOrganizationMap
    val perPatterns = KBPSlotOpenIERelationTranslator.getPersonMap
    
    val orgPatternStats = orgPatterns.iterator.toSeq flatMap {
      case (slotname, patterns) =>
        patternStats(slotname, patterns, sampleOrgs)
    }
    
    reportResults(orgPatternStats)
    println()
    println()
    
    val perPatternStats = perPatterns.iterator.toSeq flatMap {
      case (slotname, patterns) =>
        patternStats(slotname, patterns, samplePers)
    }

    reportResults(perPatternStats)
    
  }
  def reportResults(stats: Seq[(String, Long, Double)]): Unit = {
    stats foreach {
      case (wcQuery, wcNumResults, avgNumResults) =>
        println(Seq(padToEllipses(wcQuery, 75), padToEllipses(wcNumResults.toString, 15), "%.02f" format avgNumResults).mkString("\t"))
    }
    val avgAllWc = stats.map(_._2).sum / stats.size
    val avgAllSamp = stats.map(_._3).sum / stats.size
    println(Seq(padToEllipses("OVERALL AVERAGE", 75), padToEllipses(avgAllWc.toString, 15), "%.02f" format avgAllSamp).mkString("\t"))
  }

  def patternStats(slotname: String, patterns: Seq[KbpSlotToOpenIEData], sampleEntities: Set[String]): Seq[(String, Long, Double)] = {
    // map patterns to wildcard queries
    val wildcardQueries = patterns.filter(_.isValid).map { p => getQueryString("*", p) }

    // map patterns to sample query sets (for stats)
    val sampleQuerySets = patterns.filter(_.isValid).map { p =>
      sampleEntities.map { e => getQueryString(e, p) }
    }

    // build list of (wildcard query string, wildcard results, avg results over samples)
    val patternStats = wildcardQueries.zip(sampleQuerySets).map {
      case (wc, samples) =>
        val numWildcardResults = solrExec.query(wc).numResults
        val avgSampleResults = samples.map(s => solrExec.query(s).numResults).sum.toDouble / samples.size.toDouble
        (wc, numWildcardResults, avgSampleResults)
    }

    patternStats
  }

  def getQueryString(entity: String, pattern: KbpSlotToOpenIEData): String = {
    val qb = new QueryBuilder //solr query builder
    val entityIn = pattern.entityIn.getOrElse({ "" })
    if (entityIn.trim() == "arg1") {
      qb.setArg1String(entity)
    } else if (entityIn.trim() == "arg2") {
      qb.setArg2String(entity)
    } else {
      throw new Exception("entityIn contains invalid string")
    }
    qb.setRelString(pattern.openIERelationString.getOrElse({ "" }))
    val beginningOfArg2 = pattern.arg2Begins.getOrElse({ "" })
    if (beginningOfArg2 != "") {
      qb.setBeginningOfArg2String(pattern.arg2Begins.get)
    }
    val queryString = qb.getQueryString
    queryString
  }

  def padToEllipses(str: String, len: Int): String = {
    if (str.length > len - 3) {
      str.take(len - 3) + "..."
    } else {
      str.padTo(len, " ").mkString
    }
  }
}

object PatternStats {

  def main(args: Array[String]): Unit = {

    new PatternStats(args(0)).run()
  }
}