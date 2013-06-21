package edu.knowitall.tac2013.findSlotFillersApp

import jp.sf.amateras.solr.scala._
import SingleSolrQueryExecutor.issueSolrQuery



object QueryEntityForAllSlots {
  
  //takes entity string and map from KBP slot strings to Open IE relation strings and runs queries
  //to our solr instance for every type of OpenIERelation
  def executeEntityQueryForAllSlots(queryEntity: String, KBPOpenIERelationMap: Map[String,List[KbpSlotToOpenIEData]]){
    
    //for every relevant slot 
    for( pair <- KBPOpenIERelationMap){
      
      //for every different query formulation
      for(relationData <- pair._2){
        
        //make a query for each instance of relation data specifying where to search for the entity
        //string and what prepositions should be included in arg2, among other things
        
        val qb = new QueryBuilder //solr query builder
        val entityIn = relationData.EntityIn
        if (entityIn == "arg1"){
           qb.setArg1String(queryEntity)
        }
        else if (entityIn == "arg2"){
           qb.setArg2String(queryEntity)   
        }
        else{
          //error!
        }
        qb.setRelString(relationData.OpenIERelationString)
        val beginningOfArg2 = relationData.Arg2Begins
        if (beginningOfArg2 != ""){
          qb.setBeginningOfArg2String(relationData.Arg2Begins)
        }
        val queryString = qb.getQueryString
        println(queryString)
        
        //issue query (don't cut off results yet)
        val resultsMap = issueSolrQuery(queryString)
        
        //filter
        
        
        //print results
      }
      
    }
  }
}