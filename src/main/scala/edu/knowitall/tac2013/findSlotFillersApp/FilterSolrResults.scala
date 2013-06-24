package edu.knowitall.tac2013.findSlotFillersApp

object FilterSolrResults {

  //filter for arg2 beginning with proper preposition
  private def satisfiesArg2Filter(relationData: KbpSlotToOpenIEData, resultsMap: Map[String,Any] ): Boolean{

    True
  }
  
  private def satisfiesRelFilter(relationData: KbpSlotToOpenIEData, resultsMap: Map[String,Any] ): Boolean{
    
    True
  }
  
  private def satisfiesArg1Filter(relationData: KbpSlotToOpenIEData, resultsMap: Map[String,Any] ): Boolean{
    
    True
  }
  
  def filterResults(resultsArray: Array[Map[String,Any]],relationData: KbpSlotToOpenIEData) : Array[Map[String,Any]] = {
    
    var filteredResultsArray = Array[Map[String,Any]]()
    //loop over each solr result
    for(solrResultsMap <- resultsArray){
      
      //filter for arg2 beginning with proper preposition
      if (relationData.Arg2Begins != ""){
	      val solrResultsArg2 = solrResultsMap("arg2")
	      val arg2PrepositionString = relationData.Arg2Begins.trim()
	      if(solrResultsArg2.toString().substring(0,arg2PrepositionString.length) == arg2PrepositionString ||
	          solrResultsArg2.toString().substring(1,arg2PrepositionString.length+1) == arg2PrepositionString)
	        filteredResultsArray = filteredResultsArray :+ solrResultsMap
	      }
      //filter that ensures relation phrases end with query verb
      
      else{
        filteredResultsArray = filteredResultsArray :+ solrResultsMap
      }
      
      if( satisfiesArg2Filter(relationData,solrResultsMap) &&
          satisfiesArg1Filter(relationData,solrResultsMap) &&
          satisfiesRelFilter(relationData,solrResultsMap)){
          
      }
      
      
       
    }
    
    //sort array by confidence
    
    filteredResultsArray
  }
}