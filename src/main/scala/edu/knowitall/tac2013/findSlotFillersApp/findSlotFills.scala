
package edu.knowitall.tac2013.findSlotFillersApp

import KBPSlotOpenIERelationTranslator.getOrganizationMap

object findSlotFills {
  
  def main(args: Array[String]){

     assert(args.length == 2, 
         "there should be two arguments: entity name and semantic type (organization or person)")
         
     val entityName = args(0)
     val semanticType = args(1)
     println(entityName)
     println(semanticType)
     
     val orgMap = getOrganizationMap()
     for( om <- orgMap){
       println(om)
     }
     
     
       
    
  }

}