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
      
      // rank best result from each group according to a confidence measure
      // in descending order
      val bestResults = groups.values.flatMap({ extractions =>
        extractions.headOption map { headExtr =>
          (headExtr, headExtr.extr.confidence + (extractions.size  * 0.1))
        }
      }).toSeq.sortBy(-_._2)
      
      bestResults.map(_._1).take(maxAnswers)
    }
  }
  
  def getMergedGroups(slot: Slot, candidates: Seq[Candidate]): Map[String, Seq[Candidate]] = {
    val candidateEnum = candidates.toIndexedSeq
    def idsToCandidates(ids: Iterable[Int]) = ids.map(candidateEnum(_)).toSeq
    
    
    // expand to fill token => candidate (making duplicates temporarily, tracking them by index)
    val tokens = candidates.zipWithIndex.flatMap { case (candidate, index) =>
      val tokens = candidate.trimmedFill.string.split(" ")
      tokens.tails.toSeq.dropRight(1).map(tail => (tail.mkString(" "), index))
    }
    val groups = tokens.groupBy(_._1).map(p => (p._1, p._2.map(_._2).toSet)).toSeq.sortBy(-_._2.size)
    var disjointGroups = List.empty[(String, Set[Int])]
    
    fmt.printFillGroups(slot, groups.map { case (token, newIds) => (token, idsToCandidates(newIds)) })
    
    for ((token, newIds) <- groups) {
      val isDisjoint = disjointGroups.forall { case (tok, presentIds) => presentIds.forall(id => !newIds.contains(id)) }
      if (isDisjoint) disjointGroups = (token, newIds) :: disjointGroups 
    }
    
    val disjointCandidates =  disjointGroups.map { case (token, newIds) => 
      val candidates = idsToCandidates(newIds)
      val bestString = getBestFill(candidates).string
      (bestString, candidates) 
    }
    fmt.printFillGroups(slot, disjointCandidates)
    
    disjointCandidates.toMap
  }
  
  def getBestFill(candidates: Seq[Candidate]): TrimmedFill = {
    // get the most common 2-token name if one exists,
    // else the most common one-common name,
    // else the most common <any name>
    val twoTokens = candidates.filter(_.trimmedFill.string.split(" ").length == 2)
    lazy val oneTokens = candidates.filter(_.trimmedFill.string.split(" ").length == 1)
    if (twoTokens.nonEmpty) {
      twoTokens.groupBy(_.trimmedFill.string).maxBy(_._2.size)._2.head.trimmedFill
    }
    else if (oneTokens.nonEmpty) {
      oneTokens.groupBy(_.trimmedFill.string).maxBy(_._2.size)._2.head.trimmedFill
    } else {
      candidates.groupBy(_.trimmedFill.string).maxBy(_._2.size)._2.head.trimmedFill
    }
  }
}

object SlotFillReranker {
  
  
}