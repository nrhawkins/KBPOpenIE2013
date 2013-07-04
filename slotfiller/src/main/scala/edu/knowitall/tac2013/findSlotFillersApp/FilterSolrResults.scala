package edu.knowitall.tac2013.findSlotFillersApp

import edu.knowitall.tac2013.openie.KbpExtraction
import QueryType._

object FilterSolrResults {

  //filter for arg2 beginning with proper preposition
  private def satisfiesArg2PrepositionFilter(sourceSet: CandidateSet, extraction: KbpExtraction): Boolean = {

    sourceSet.pattern.arg2Begins match {
      case Some(arg2PrepositionString) => {
        val solrResultsArg2 = extraction.arg2.originalText
        (solrResultsArg2.toString().toLowerCase().substring(0, arg2PrepositionString.length) == arg2PrepositionString.toLowerCase())
      }
      case None => true
    }
  }

  private def satisfiesRelFilter(relationData: SlotPattern, extraction: KbpExtraction): Boolean = {

    if (relationData.isValid) {

      if (!relationData.openIERelationString.get.contains("<JobTitle>")) {

        val relationTerms = relationData.openIERelationString.get.trim().split(" ")

        val relationTermsReversed = relationTerms.reverse

        val relationTermsFromExtraction = extraction.rel.originalText.trim().split(" ")

        var count = 0
        for (term <- relationTermsReversed) {
          val sentenceWord = relationTermsFromExtraction(relationTermsFromExtraction.length - 1 - count)
          if ((term.toLowerCase() != sentenceWord.toLowerCase()) &&
            (term.replace("[", "").replace("]", "").toLowerCase() != sentenceWord.replace("[", "").replace("]", "").toLowerCase())) {
            return false
          }
          count = count + 1
        }
        return true
      } else {
        val chunkedSentence = extraction.sentence.chunkedTokens
        val types = SemanticTaggers.useJobTitleTagger(chunkedSentence)
        val relLocation = extraction.rel.tokenInterval

        for (t <- types) {
          if (t.interval().intersects(relLocation)) {
            return true
          }
        }
        return false
      }
    } else {

      throw new Exception("KbpSlotToOpenIEData instance is not valid.")
      false
    }
  }

  private def satisfiesEntityFilter(sourceSet: CandidateSet, queryType: CandidateType, extraction: KbpExtraction, queryEntity: String): Boolean = {

    val pattern = sourceSet.pattern
    
    if (queryType == QueryType.REGULAR) {
      if (pattern.isValid) {

        val entityIn = pattern.entityIn.get.trim()
        val entityFromExtraction = entityIn.toLowerCase() match {
          case "arg1" => extraction.arg1.originalText
          case "arg2" => extraction.arg2.originalText
          case _ => throw new Exception("Poorly formatted entityIn field, should be arg1 or arg2")
        }

        val entityFromExtractionSplit = entityFromExtraction.toString().split(" ")

        val queryEntityReversedSplit = queryEntity.trim().split(" ").reverse

        var count = 0
        for (term <- queryEntityReversedSplit) {
          val extractionWord = entityFromExtractionSplit(entityFromExtractionSplit.length - 1 - count)
          if (term.toLowerCase() != extractionWord.toLowerCase()) {
            //(term.toLowerCase() != extractionWord.substring(1,extractionWord.length-1).toLowerCase())){
            return false
          }

          count = count + 1
        }

        true
      } else {

        throw new Exception("KbpSlotToOpenIEData instance is not valid.")
        false
      }
    } else {
      true
    }
  }

  private def satisfiesSemanticFilter(relationData: SlotPattern, extraction: KbpExtraction): Boolean = {

    val slotType = relationData.slotType.getOrElse({ "" })
    val slotLocation = relationData.slotFillIn match {
      case Some("arg1") => extraction.arg1.tokenInterval
      case Some("arg2") => extraction.arg2.tokenInterval
      case Some("relation") => extraction.rel.tokenInterval
      case _ => return false
    }

    val chunkedSentence = extraction.sentence.chunkedTokens

    if (slotType == "Organization" || slotType == "Person" || slotType == "Stateorprovince" ||
      slotType == "City" || slotType == "Country") {
      val types = SemanticTaggers.useStandfordNERTagger(chunkedSentence)

      for (t <- types) {
        if (t.interval().intersects(slotLocation)) {
          slotType match {
            case "Organization" => {
              if (t.descriptor() == "StanfordORGANIZATION") {
                return true
              }
            }
            case "Person" => {
              if (t.descriptor() == "StanfordPERSON") {
                return true
              }
            }
            // default case will be location
            case _ => {
              if (t.descriptor() == "StanfordLOCATION") {
                return true
              }
            }
          }
        }
      }

      return false
    } else if (slotType == "School") {

      val types = SemanticTaggers.useEducationalOrganizationTagger(chunkedSentence)

      for (t <- types) {
        if (t.interval().intersects(slotLocation)) return true

      }

      return false

    } else if (slotType == "JobTitle") {

      val types = SemanticTaggers.useJobTitleTagger(chunkedSentence)

      for (t <- types) {
        if (t.interval().intersects(slotLocation)) return true

      }

      return false

    } else if (slotType == "Nationality") {

      val types = SemanticTaggers.useNationalityTagger(chunkedSentence)

      for (t <- types) {
        if (t.interval().intersects(slotLocation)) return true

      }

      return false

    } else if (slotType == "Religion") {

      val types = SemanticTaggers.useReligionTagger(chunkedSentence)

      for (t <- types) {
        if (t.interval().intersects(slotLocation)) return true

      }

      return false

    } else if (slotType == "Date") {
      val types = SemanticTaggers.useDateTagger(chunkedSentence)

      for (t <- types) {
        if (t.interval().intersects(slotLocation)) return true

      }

      return false

    } else {

      return true

    }

  }

  //filters results from solr by calling helper methods that look at the KbpSlotToOpenIEData specifications and compare
  //that data with the results from solr to see if the relation is still a candidate
  //
  def filterResults(candidateSet: CandidateSet, queryEntity: String): CandidateSet = {

    //loop over each solr result
    val pattern = candidateSet.pattern

    val filteredResults = candidateSet.extractionsMap.map { case (queryType, extractions) =>
        val filteredExtractions = extractions.filter { extr =>
          (satisfiesArg2PrepositionFilter(candidateSet, extr) &&
            satisfiesEntityFilter(candidateSet, queryType, extr, queryEntity) &&
            satisfiesRelFilter(pattern, extr) &&
            satisfiesSemanticFilter(pattern, extr))
        }
        (queryType, filteredExtractions)
    }
    
    new CandidateSet(pattern, filteredResults)
  }
}