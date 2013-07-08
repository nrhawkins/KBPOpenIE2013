package edu.knowitall.tac2013.findSlotFillersApp

import edu.knowitall.tac2013.openie.KbpExtraction

/**
 * Finds fills given candidates for a particular slot.
 */
class SlotFillReranker(fmt: OutputFormatter) {

  import SlotFillReranker._
  
  /**
   * Requires that all candidates (if any) are for the same slot.
   */
  def findSlotAnswers(slot: Slot, kbpQuery: KBPQuery, slotCandidates: Seq[Candidate]): Seq[Candidate] = {
    
    if (slotCandidates.isEmpty) List.empty
    
    else {
      
      val maxAnswers = slot.maxResults
      
      require(slotCandidates.forall(_.pattern.slotName.equals(slot.name)))
      
      val trimGroups = slotCandidates.groupBy(_.trimmedFill.string)
      
      fmt.printFillGroups("Raw trim groups:", slot, trimGroups)
      
      val prefixesMerged = mergePrefixes(trimGroups)
      
      fmt.printFillGroups("Prefix trim groups merged:", slot, prefixesMerged)
      
      val groups = getSuffixGroups(slot, prefixesMerged)
      
      fmt.printFillGroups("Largest disjoint suffix groups:", slot, groups)
      
      // rank extractions my trimFill frequency * trimFill length
      val rankedAnswers = groups.iterator.toSeq.sortBy(-_._2.size).map { case (key, candidates) =>
        val trimGroups = candidates.groupBy(_.trimmedFill.string)
        val sortedTrimGroups = trimGroups.toSeq.sortBy { case (trim, candidates) => -trim.length * candidates.size } 
        val sortedCandidates = sortedTrimGroups.flatMap { case (trim, candidates) => candidates }
        (sortedCandidates.head.trimmedFill.string, sortedCandidates)
      }
      
      fmt.printFillGroups("Ranked answer groups (descending)", slot, rankedAnswers.toMap)
      
      val bestAnswers = rankedAnswers.map(_._2.head)
      
      bestAnswers.take(maxAnswers)
    }
  }

  def isPrefixOf(key1: String, key2: String) = key1 != key2 && key2.startsWith(key1)
  def isSuffixOf(key1: String, key2: String) = key1 != key2 && key2.endsWith(key1)

  def mergePrefixes(trimGroups: Map[String, Seq[Candidate]]): Map[String, Seq[Candidate]] = {
    mergePairwise(trimGroups, isPrefixOf)
  }
  
  def mergePairwise(trimGroups: Map[String, Seq[Candidate]], pairEqTest: (String, String) => Boolean): Map[String, Seq[Candidate]] = {
    
    var mergedGroups = trimGroups
    
    // for each key in trimGroups, see if it is a substring of another.
    var changed = true
    while (changed) {
      val allKeyPairs = mergedGroups.keys.flatMap { key1 =>
        mergedGroups.keys.map(key2 => (removeStopTokens(key1), removeStopTokens(key2))) 
      }
      allKeyPairs find pairEqTest.tupled match {
        case Some((key1, key2)) => {
          val fullGroup = mergedGroups(key1) ++ mergedGroups(key2)
          // remove both prefix and string and add the new merged group under key string
          mergedGroups --= Seq(key1, key2) 
          mergedGroups += (key2 -> fullGroup)
          changed = true
        }
        case None => changed = false
      }
    }
    mergedGroups.toMap
  }
  
  /**
   * Expand to non-disjoint mapping from (token suffix) => candidates with that fill suffix.
   * Filter out stop tokens like "Inc., Corp., Mr.", etc.
   */
  def groupByFillSuffixes(trimGroups: Map[String, Seq[Candidate]]): Map[String, Seq[Candidate]] = {

    val tokenGroups = trimGroups.toSeq.flatMap { case (trim, candidates) =>
      val tokens = removeStopTokens(trim).split(" ")
      val tails = tokens.tails.toSeq.dropRight(1)
      val keys = tails.map(_.mkString(" "))
      keys map { k => (k, candidates) }
    } 
    tokenGroups.groupBy(_._1).map { case (key, group) =>
      (key, group.flatMap { case (_, candidates) => candidates })
    }
  }
  
  def getSuffixGroups(slot: Slot, trimGroups: Map[String, Seq[Candidate]]): Map[String, Seq[Candidate]] = {

    val suffixGroups = groupByFillSuffixes(trimGroups)
    
    var disjointIds = Set.empty[Int]
    var disjointGroups = Map.empty[String, Seq[Candidate]]
    
    fmt.printFillGroups("Trim suffix groups, non-disjoint", slot, suffixGroups)
    
    // Iterate over groups in descending order of size, choosing a greedily minimal disjoint subset of the original groups
    var biggestGroups = suffixGroups.toSeq.sortBy(-_._2.size)
    
    while (!biggestGroups.isEmpty) {
      val (key, candidates) = biggestGroups.head      
      // insert all candidates
      disjointIds ++= candidates.map(_.id)
      disjointGroups += (key -> candidates)
      // filter these candidates from all remaining groups and re-sort.
      val remainingGroups = biggestGroups.tail
      val filtered = remainingGroups.map { case (key, candidates) => 
        (key, candidates.filter(c => !disjointIds.contains(c.id))) 
      } filter(_._2.nonEmpty)
      biggestGroups = filtered.sortBy(-_._2.size)
    }
    
    for ((token, newCandidates) <- suffixGroups.toSeq.sortBy(-_._2.size)) {
      val newIds = newCandidates.map(_.id)
      val isDisjoint = disjointIds.forall { id => !newIds.contains(id) }
      if (isDisjoint) {
        disjointIds ++= newIds
        disjointGroups += (token -> newCandidates) 
      }
    }
    
    disjointGroups
  }
}

object SlotFillReranker {
  
  def removeStopTokens(str: String): String = {
    var result = str
    for (stop <- stopTokens) result.replaceAll(stop, "")
    result.trim
  }
  
  val orgAbbreviations = Set("corp", "inc", "co", "corporation", "incorporated")
  val stopTokens = orgAbbreviations ++ Set("mr", "mrs", "ms", "\\.")
  
}