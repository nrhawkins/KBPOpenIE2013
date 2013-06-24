package edu.knowitall.tac2013.findSlotFillersApp

object FilterSolrResults {

  //filter for arg2 beginning with proper preposition
  private def satisfiesArg2Filter(relationData: KbpSlotToOpenIEData, resultsMap: Map[String,Any] ): Boolean ={
      if (relationData.Arg2Begins != ""){
	      val solrResultsArg2 = resultsMap("arg2")
	      val arg2PrepositionString = relationData.Arg2Begins.trim()
	      
	      if(solrResultsArg2.toString().substring(0,arg2PrepositionString.length) == arg2PrepositionString ||
	          solrResultsArg2.toString().substring(1,arg2PrepositionString.length+1) == arg2PrepositionString)
	        true
	      
      	  else{
            false
          }
      }
      else{
        true
      }
  }
  
  private def satisfiesRelFilter(relationData: KbpSlotToOpenIEData, resultsMap: Map[String,Any] ): Boolean ={
    val relationTerms = relationData.OpenIERelationString.trim().split(" ")
    
    val relationTermsReversed =  relationTerms.reverse
    
    val relationTermsFromExtraction = resultsMap("rel").toString.trim().split(" ")
    
    var count = 0 
    for(term <- relationTermsReversed){
      val sentenceWord = relationTermsFromExtraction(relationTermsFromExtraction.length-1-count)
      if( (term != sentenceWord) &&
          (term != sentenceWord.substring(1,sentenceWord.length-1))){
        return false
      }
      count = count +1
    }
    
    
    true
  }
  
  private def satisfiesArg1Filter(relationData: KbpSlotToOpenIEData, resultsMap: Map[String,Any] ): Boolean ={
    
    true
  }
  
  def filterResults(resultsArray: Array[Map[String,Any]],relationData: KbpSlotToOpenIEData) : Array[Map[String,Any]] = {
    
    var filteredResultsArray = Array[Map[String,Any]]()
    //loop over each solr result
    for(solrResultsMap <- resultsArray){
      
//      //filter for arg2 beginning with proper preposition
//      if (relationData.Arg2Begins != ""){
//	      val solrResultsArg2 = solrResultsMap("arg2")
//	      val arg2PrepositionString = relationData.Arg2Begins.trim()
//	      if(solrResultsArg2.toString().substring(0,arg2PrepositionString.length) == arg2PrepositionString ||
//	          solrResultsArg2.toString().substring(1,arg2PrepositionString.length+1) == arg2PrepositionString)
//	        filteredResultsArray = filteredResultsArray :+ solrResultsMap
//	      }
//      //filter that ensures relation phrases end with query verb
//      
//      else{
//        filteredResultsArray = filteredResultsArray :+ solrResultsMap
//      }
      
      //check that the result passes all the filters
      
      if( satisfiesArg2Filter(relationData,solrResultsMap) &&
          satisfiesArg1Filter(relationData,solrResultsMap) &&
          satisfiesRelFilter(relationData,solrResultsMap)){
        
          filteredResultsArray = filteredResultsArray :+ solrResultsMap
          
      }
      
      
       
    }
    
    //sort array by confidence
    
    filteredResultsArray
  }
}