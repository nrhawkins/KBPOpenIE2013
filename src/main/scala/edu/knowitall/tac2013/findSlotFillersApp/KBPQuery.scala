package edu.knowitall.tac2013.findSlotFillersApp

import edu.knowitall.tac2013.findSlotFillersApp.KBPEntityType._




class KBPQuery (idArg: String, nameArg: String, docArg: String,
    begOffsetArg: Int, endOffsetArg: Int, entityTypeArg: KBPEntityType,
    nodeIdArg: String, slotsToFillArg: List[String]){
  
  private val id = idArg
  private val name = nameArg
  private val docID = docArg
  private val begOffset = begOffsetArg
  private val endOffset = endOffsetArg
  private val entityType = entityTypeArg
  private val nodeId = nodeIdArg
  private val slotsToFill = slotsToFillArg
  
}

object KBPQuery {

  import KBPEntityType._

  
  def parseKBPQuery(pathToFile: String): KBPQuery = {
    
    println("parseKBPQuery")
    new KBPQuery("0","0","0",0,0,ORG,"0",List("hello"))
  }
  
}