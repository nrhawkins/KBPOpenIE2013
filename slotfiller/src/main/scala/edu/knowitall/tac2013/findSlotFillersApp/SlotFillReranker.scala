package edu.knowitall.tac2013.findSlotFillersApp

import edu.knowitall.tac2013.openie.KbpExtraction

/**
 * Finds fills given candidates for a particular slot.
 */
class SlotFillReranker(fmt: OutputFormatter) {

  /**
   * Requires that all candidates (if any) are for the same slot.
   */
  def findSlotAnswers(slot: Slot, kbpQuery: KBPQuery, slotCandidates: Seq[Candidate]): Seq[Candidate] = {
    
    if (slotCandidates.isEmpty) List.empty
    
    else {
      
      val maxAnswers = slot.maxResults
      
      require(slotCandidates.forall(_.pattern.slotName.equals(slot.name)))
      
      // expand groups to 
      val groups = getMergedGroups(slot, slotCandidates)
      
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

  /**
   * Expand to non-disjoint mapping from (token suffix) => candidates with that fill suffix.
   * Filter out stop tokens like "Inc., Corp., Mr.", etc.
   */
  def groupByFillTokens(cands: Seq[Candidate]): Map[String, Seq[Candidate]] = {

    val tokens = cands.flatMap { candidate =>
      val tokens = candidate.trimmedFill.string.split(" ")
      tokens.tails.toSeq.dropRight(1).map(tail => (tail.mkString(" "), candidate))
    }
    val groupedByTokens = tokens.groupBy(_._1).map(p => (p._1, p._2.map(_._2)))
    groupedByTokens
  }
  
  def getMergedGroups(slot: Slot, candidates: Seq[Candidate]): Map[String, Seq[Candidate]] = {

    val groups = groupByFillTokens(candidates)
    
    var disjointIds = Set.empty[Int]
    var disjointGroups = Map.empty[String, Seq[Candidate]]
    
    fmt.printFillGroups("Trim suffix groups, non-disjoint", slot, groups)
    
    // Iterate over groups in descending order of size, choosing a greedily minimal disjoint subset of the original groups
    var biggestGroups = groups.toSeq.sortBy(-_._2.size)
    
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
    
    for ((token, newCandidates) <- groups.toSeq.sortBy(-_._2.size)) {
      val newIds = newCandidates.map(_.id)
      val isDisjoint = disjointIds.forall { id => !newIds.contains(id) }
      if (isDisjoint) {
        disjointIds ++= newIds
        disjointGroups += (token -> newCandidates) 
      }
    }
    
    fmt.printFillGroups("Trim suffix groups, disjoint", slot, disjointGroups)
    
    disjointGroups
  }
}

object SlotFillReranker {
  
  
}