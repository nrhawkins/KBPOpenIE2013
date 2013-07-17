package edu.knowitall.tac2013.app

import edu.knowitall.tac2013.openie.KbpExtraction
import edu.knowitall.tac2013.app.LocationHelper.findLocationTaggedType
import edu.knowitall.tac2013.solr.query.SolrQueryType._
import edu.knowitall.tac2013.solr.query.SolrQueryType
import edu.knowitall.collection.immutable.Interval
import scala.util.matching.Regex

object FilterSolrResults {
  
  lazy val semanticCategoryPattern = new Regex("[A-Z<>]\\w+")

  
  private def satisfiesRequirementAtInterval(start: Integer, end: Integer, specificationStrings: Array[String], candidate: Candidate ): Boolean = {
    
        var intervalHead = start
        for(specificationString <- specificationStrings){
          //if the string is a semantic category requirement string
          if(intervalHead >= end){
            System.out.println("Returns false because of token interval")
            return false
          }
          if (semanticCategoryPattern.findFirstIn(specificationString).isDefined){
            val newIntervalHead = updateIntervalHead(specificationString,candidate,Interval.closed(intervalHead,end),false)
            if(newIntervalHead.isDefined){
              intervalHead =  newIntervalHead.get.end
            }
            else{
              return false
            }
          }
          //if the string is simply a word to match
          else{
            val extractionTokenWithoutBrackets = candidate.extr.sentence.chunkedTokens(Interval.closed(intervalHead, intervalHead+1)).head.string.replace("[", "").replace("]", "").toLowerCase()
   
            if(specificationString.toLowerCase().trim() == extractionTokenWithoutBrackets){
              System.out.println("EQUAL: " + specificationString.toLowerCase().trim() + " and " + extractionTokenWithoutBrackets)
              intervalHead = intervalHead+1
            }
            else{
              System.out.println("NOT EQUAL: " + specificationString.toLowerCase().trim() + " and " + extractionTokenWithoutBrackets)
              return false
            }
          }
        }
        return true
  }
  
  
  private def filterOutBrackets(specificationStrings: Array[String], rel: String): Option[Array[String]] = {
    
    var newLength = specificationStrings.length
    val relWords = rel.split(" ")
    if(relWords.last.contains("[")){
      if(relWords.last.replace("[", "").replace("]", "").trim().toLowerCase() == specificationStrings.last){
        return Some(specificationStrings.take(specificationStrings.length-1))
      }
      else{
        return None
      }
    }
    else{
      Some(specificationStrings)
    }
    
  }
  private def satisfiesRequirementAtIntervalFromEnd(start: Integer, end: Integer, specificationStrings: Array[String], candidate: Candidate ): Boolean = {
        val remainingSpecificationStringsOption = filterOutBrackets(specificationStrings,candidate.extr.rel.originalText)
        if(remainingSpecificationStringsOption.isEmpty) return false
        var intervalTail = end
        for(specificationString <- remainingSpecificationStringsOption.get.reverse){
          //if the string is a semantic category requirement string
          if(start >= intervalTail){
            System.out.println("Returns false because of token interval")
            return false
          }
          if (semanticCategoryPattern.findFirstIn(specificationString).isDefined){
            System.out.println("Semantic Specification string is :" + specificationString)
            val newIntervalTail = updateIntervalHead(specificationString,candidate,Interval.open(start,intervalTail),true)
            if(newIntervalTail.isDefined){
              intervalTail =  newIntervalTail.get.start
            }
            else{
              System.out.println("Semantic Tag for " + specificationString + " not found, so returning false")
              return false
            }
          }
          //if the string is simply a word to match
          else{
            val extractionTokenWithoutBrackets = candidate.extr.sentence.chunkedTokens(Interval.closed(intervalTail-1, intervalTail)).head.string.replace("[", "").replace("]", "").toLowerCase()
   
            if(specificationString.toLowerCase().trim() == extractionTokenWithoutBrackets){
              System.out.println("EQUAL: " + specificationString.toLowerCase().trim() + " and " + extractionTokenWithoutBrackets)
              intervalTail = intervalTail -1 
            }
            else{
              System.out.println("NOT EQUAL: " + specificationString.toLowerCase().trim() + " and " + extractionTokenWithoutBrackets)
              return false
            }
          }
        }
        return true
  }
  
  private def intervalMatches(t: Interval, target: Interval, backwards: Boolean): Boolean = {
     
    if(backwards){
      if(t.end == target.end){
        return true
      }
      else{
        return false
      }
      
    }
    else{
      if(t.start == target.start){
        return true
      }
      else{ return false}
    }
  }
  
  private def updateIntervalHead(semanticTypeRaw: String, candidate: Candidate, interval :Interval, backwards: Boolean ): Option[Interval] = {

    val semanticType = semanticTypeRaw.replace("<","").replace(">", "").trim()
    
    val chunkedSentence = candidate.extr.sentence.chunkedTokens
   
    
    if (semanticType == "Organization") {
      val types = SemanticTaggers.useStandfordNERTagger(chunkedSentence)
      for (t <- types) {
         if (t.descriptor() == "StanfordORGANIZATION") {
             if(intervalMatches(t.interval,interval,backwards)){
                return Some(t.interval())
             }
            }
      }
      return None
    } else if(semanticType == "Person"){
      val types = SemanticTaggers.useStandfordNERTagger(chunkedSentence)
      for (t <- types) {
        if(intervalMatches(t.interval,interval,backwards)) {
            if (t.descriptor() == "StanfordPERSON") {
                return Some(t.interval())
            }
        }
      }
      return None
    }
     else if(semanticType == "Location"){
      val types = SemanticTaggers.useStandfordNERTagger(chunkedSentence)
      for (t <- types) {
        if(intervalMatches(t.interval,interval,backwards)) {
            if (t.descriptor() == "StanfordLOCATION") {
                return Some(t.interval())
            }
        }
      }
      return None
      
    }
    else if (semanticType == "School") {
      val types = SemanticTaggers.useEducationalOrganizationTagger(chunkedSentence)
      for (t <- types) {
        if(intervalMatches(t.interval,interval,backwards)) return Some(t.interval())
      }

      return None

    } else if (semanticType == "JobTitle") {
      println("Looking for JobTitle")
      val types = SemanticTaggers.useJobTitleTagger(chunkedSentence)
      println("Number of tags = " + types.length)
      println("Target Interval = " + interval)
      for (t <- types) {
        println("Tagged Interval = " + t.interval())
        if(intervalMatches(t.interval,interval,backwards)) return Some(t.interval())
      }

      return None

    } else if (semanticType == "Nationality") {

      val types = SemanticTaggers.useNationalityTagger(chunkedSentence)
      for (t <- types) {
        if(intervalMatches(t.interval,interval,backwards)) return Some(t.interval())

      }

      return None

    } else if (semanticType == "Religion") {
      val types = SemanticTaggers.useReligionTagger(chunkedSentence)

      for (t <- types) {
        if(intervalMatches(t.interval,interval,backwards)) return Some(t.interval())
      }

      return None

    } else if (semanticType == "Date") {
      val types = SemanticTaggers.useDateTagger(chunkedSentence)
      
      for (t <- types) {
        if(intervalMatches(t.interval,interval,backwards)) return Some(t.interval())

      }

      return None

    } else if (semanticType == "ProperNoun"){
      //need to figure out what to do for general ProperNoun semantic filter
      
      return None
    }  else if ((semanticType =="<integer>-year-old") || (semanticType == "Integer")){

      val types = SemanticTaggers.useIntegerTagger(chunkedSentence)
      for (t <- types) {
        if(intervalMatches(t.interval,interval,backwards)) return Some(t.interval())

      }

      return None
      
    } else {
    
    

      return None

    }

    
  }

  //filter for arg2 beginning with proper preposition
  private def satisfiesArg2BeginsFilter(candidate: Candidate): Boolean = {
    

    candidate.pattern.arg2Begins match {
      case Some(arg2BeginsString) => {
        
        val patternSpecificationStrings = arg2BeginsString.split(" ")
        val intervalHead = candidate.extr.arg2.tokenInterval.start
        val end = candidate.extr.arg2.tokenInterval.end
        satisfiesRequirementAtInterval(intervalHead, end, patternSpecificationStrings, candidate)
        
        
      }
//        for(arg2RequirementString <- patternSpecificationStrings){
//          //if the string is a semantic category requirement string
//          if(intervalHead >= candidate.extr.arg2.tokenInterval.end){
//            System.out.println("Returns false because of token interval")
//            return false
//          }
//          if (semanticCategoryPattern.findFirstIn(arg2RequirementString).isDefined){
//            val newIntervalHead = updateIntervalHead(arg2RequirementString,candidate,Interval.closed(intervalHead,candidate.extr.arg2.tokenInterval.end))
//            if(newIntervalHead.isDefined){
//              intervalHead =  newIntervalHead.get.start
//            }
//            else{
//              return false
//            }
//          }
//          //if the string is simply a word to match
//          else{
//            val extractionTokenWithoutBrackets = candidate.extr.sentence.chunkedTokens(Interval.closed(intervalHead, intervalHead+1)).head.string.replace("[", "").replace("]", "").toLowerCase()
//   
//            if(arg2RequirementString.toLowerCase().trim() == extractionTokenWithoutBrackets){
//              System.out.println("EQUAL: " + arg2RequirementString.toLowerCase().trim() + " and " + extractionTokenWithoutBrackets)
//              intervalHead = intervalHead+1
//            }
//            else{
//              System.out.println("NOT EQUAL: " + arg2RequirementString.toLowerCase().trim() + " and " + extractionTokenWithoutBrackets)
//              return false
//            }
//          }
//        }
//        System.out.println("Arg2Begins filter returns true")
//        return true
//      }
      case None => true
    }
  }

  private def satisfiesRelFilter(candidate: Candidate): Boolean = {

    
    //if there are no semantic strings in the relation condidtion then count backwards for
    //the start interval and call satisfiesRequirementAtInterval
    
    

    candidate.pattern.relString match {
      case Some(relString) => {
        
        val patternSpecificationStrings = relString.split(" ")
        val intervalHead = candidate.extr.rel.tokenInterval.start
        val end = candidate.extr.rel.tokenInterval.end
        val b = satisfiesRequirementAtIntervalFromEnd(intervalHead, end, patternSpecificationStrings, candidate)
        println("RelString passes? = " + b)
        b
      }
      case None => true
    }
    
    
    
//    val pattern = candidate.pattern
//    
//    if (pattern.isValid) {
//
//      if (!pattern.relString.get.contains("<JobTitle>")) {
//
//        val relationTerms = pattern.relString.get.trim().split(" ")
//
//        val relationTermsReversed = relationTerms.reverse
//
//        val relationTermsFromExtraction = candidate.extr.rel.originalText.trim().split(" ")
//
//        var count = 0
//        for (term <- relationTermsReversed) {
//          val sentenceWord = relationTermsFromExtraction(relationTermsFromExtraction.length - 1 - count)
//          if ((term.toLowerCase() != sentenceWord.toLowerCase()) &&
//            (term.replace("[", "").replace("]", "").toLowerCase() != sentenceWord.replace("[", "").replace("]", "").toLowerCase())) {
//            return false
//          }
//          count = count + 1
//        }
//        return true
//      } else {
//        val chunkedSentence = candidate.extr.sentence.chunkedTokens
//        val types = SemanticTaggers.useJobTitleTagger(chunkedSentence)
//        val relLocation = candidate.extr.rel.tokenInterval
//
//        for (t <- types) {
//          if (t.interval().intersects(relLocation)) {
//            return true
//          }
//        }
//        return false
//      }
//    } else {
//
//      throw new Exception("KbpSlotToOpenIEData instance is not valid.")
//      false
//    }
  }
  
  private def satisfiesTermFilters(candidate: Candidate) : Boolean = {
    
    //arg1 Terms
    val arg1TermsSatisfied =
      candidate.pattern.arg1Terms match {
        case Some(arg1Terms) => {
              val arg1Interval = candidate.extr.arg1.tokenInterval
              val patternSpecificationStrings = arg1Terms.split(" ")
              var foundMatchingSubsequence = false
              for(x  <- arg1Interval){
            	  if(satisfiesRequirementAtInterval(x,arg1Interval.end,patternSpecificationStrings,candidate)){
            	    foundMatchingSubsequence = true
            	  }
              }
              foundMatchingSubsequence
        }
        case None => true
      }


    //arg2 Terms
    val arg2TermsSatisfied =
      candidate.pattern.arg2Terms match {
        case Some(arg2Terms) => {
              val arg2Interval = candidate.extr.arg2.tokenInterval
              val patternSpecificationStrings = arg2Terms.split(" ")
              var foundMatchingSubsequence = false
              for(x  <- arg2Interval){
            	  if(satisfiesRequirementAtInterval(x,arg2Interval.end,patternSpecificationStrings,candidate)){
            	    foundMatchingSubsequence = true
            	  }
              }
              foundMatchingSubsequence
        }
        case None => true
      }
    
    (arg1TermsSatisfied && arg2TermsSatisfied)
  }

  private def satisfiesEntityFilter(kbpQuery: KBPQuery)(candidate: Candidate): Boolean = {

    val pattern = candidate.pattern
    val queryEntity = kbpQuery.name
    
    if (candidate.queryType == SolrQueryType.REGULAR) {
      if (pattern.isValid) {

        val entityIn = pattern.entityIn.get.trim()
        val entityFromExtraction = entityIn.toLowerCase() match {
          case "arg1" => candidate.extr.arg1.originalText
          case "arg2" => candidate.extr.arg2.originalText
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
    }
    else if (candidate.queryType == SolrQueryType.COREF){
      //filter out if the entity slot contains a wikiLinkNodeID
      candidate.pattern.entityIn.getOrElse({""}) match {
        case "arg1" => { if(candidate.extr.arg1.wikiLink.isDefined) return false}
        case "arg2" => { if(candidate.extr.arg2.wikiLink.isDefined) return false}
      }
      
      //filter out if there is no mention of the target entity in the current sentence or the preceeding two sentences
      val sentencesWhereEntityIsMentioned = kbpQuery.docIdToSentNumDocIdPairMap(candidate.extr.sentence.docId).map(x => x._2)
      val thisSentenceNum = candidate.extr.sentence.sentNum
      val sentenceRange = thisSentenceNum-2 to thisSentenceNum
      for(sentenceNum <- sentencesWhereEntityIsMentioned){
        if(sentenceRange.contains(sentenceNum)){
          return true
        }
      }
      
      //if there is no sentence with a mention of the entity in the specified range
      //around the coref extraction then the candidate extraction should be filtered out.
      false
    }
    
    else {
      true
    }
  }
  

  private def satisfiesSemanticFilter(candidate: Candidate): Boolean = {

    val pattern = candidate.pattern
    val types = candidate.types
    val trimmedFill = candidate.trimmedFill
    val slotType = pattern.slotType.getOrElse({ "" })
    val slotLocation = pattern.slotFillIn match {
      case Some("arg1") => candidate.extr.arg1.tokenInterval
      case Some("arg2") => candidate.extr.arg2.tokenInterval
      case Some("relation") => candidate.extr.rel.tokenInterval
      case _ => return false
    }

    val chunkedSentence = candidate.extr.sentence.chunkedTokens

    if (slotType == "Organization" || slotType == "Person" || slotType == "Stateorprovince" ||
      slotType == "City" || slotType == "Country") {

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
              //if trimmed Fill does not exist then the Candidate
              //constructor has filtered out this extraction
              val typesInSlotFill = types.filter(t => (t.interval().intersects(slotLocation)))
              if(findLocationTaggedType(typesInSlotFill,slotType).isDefined){
                return true
              }
              else{
                return false
              }

            }
          }
        }
      }

      return false
    } else if (slotType == "School") {
      
      for (t <- types) {
        if (t.interval().intersects(slotLocation)) return true

      }

      return false

    } else if (slotType == "JobTitle") {


      for (t <- types) {
        if (t.interval().intersects(slotLocation)) return true

      }

      return false

    } else if (slotType == "Nationality") {


      for (t <- types) {
        if (t.interval().intersects(slotLocation)) return true

      }

      return false

    } else if (slotType == "Religion") {



      for (t <- types) {
        if (t.interval().intersects(slotLocation)) return true

      }

      return false

    } else if (slotType == "Date") {

      for (t <- types) {
        if (t.interval().intersects(slotLocation)) return true

      }

      return false

    } else if (slotType == "ProperNoun"){
      //need to figure out what to do for general ProperNoun semantic filter
      
      return true
    }  else if ((slotType =="<integer>-year-old") || (slotType == "Integer")){

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
  def filterResults(unfiltered: Seq[Candidate], kbpQuery: KBPQuery): Seq[Candidate] = {
    
    def combinedFilter(candidate: Candidate) = (
            satisfiesArg2BeginsFilter(candidate) &&
            satisfiesEntityFilter(kbpQuery)(candidate) &&
            satisfiesRelFilter(candidate) &&
            satisfiesSemanticFilter(candidate) &&
            satisfiesTermFilters(candidate))
    
    unfiltered filter combinedFilter
  }
}