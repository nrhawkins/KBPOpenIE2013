
package edu.knowitall.tac2013.findSlotFillersApp

import KBPSlotOpenIERelationTranslator.getOrganizationMap
import KBPSlotOpenIERelationTranslator.getPersonMap
import QueryEntityForAllSlots.executeEntityQueryForAllSlots

object findSlotFills {
  
  def main(args: Array[String]){

     assert(args.length == 2, 
         "there should be two arguments: entity name and semantic type (organization or person)")
         
     val entityName = args(0).replace("_", " ")
     val semanticType = args(1)
     println(entityName)
     println(semanticType)
     
     if (semanticType == "organization"){
	     val orgMap = getOrganizationMap()
	     executeEntityQueryForAllSlots(entityName, orgMap.toMap)
  	 }
     else if (semanticType == "person"){
	     val perMap = getPersonMap()
	     executeEntityQueryForAllSlots(entityName, perMap.toMap)
     }
     
     else{
       
     }
     
     
       
    
  }

}