package edu.knowitall.tac2013.findSlotFillersApp

import edu.knowitall.tac2013.findSlotFillersApp.KBPEntityType._
//import scala.io._
import scala.xml.XML




class KBPQuery (idArg: String, nameArg: String, docArg: String,
    begOffsetArg: Int, endOffsetArg: Int, entityTypeArg: KBPEntityType,
    nodeIdArg: String, slotsToFillArg: Set[String]){
  
  private val id = idArg
  private val name = nameArg
  private val docID = docArg
  private val begOffset = begOffsetArg
  private val endOffset = endOffsetArg
  private val entityType = entityTypeArg
  private val nodeId = nodeIdArg
  private val slotsToFill = slotsToFillArg
  
  def getID = id
  def getName = name
  def getDocID = docID
  def getBegOffset = begOffset
  def getEndOffset = endOffset
  def getEntityType = entityType
  def getNodeId = nodeId
  def getSlotsToFill = slotsToFill
  
}

object KBPQuery {

  import KBPEntityType._

  
  def parseKBPQuery(pathToFile: String): KBPQuery = {
    

    //val pathToXML = Source.fromFile(pathToFile)
    
    val xml = XML.loadFile(pathToFile)
    
    val queryXMLSeq = xml.\("query")
    if(queryXMLSeq.length != 1) {
      throw new IllegalArgumentException("XML file should include only one query")
    }
    
    val queryXML = queryXMLSeq(0)
    val idText = queryXML.attribute("id") match 
    		{case Some(id) if id.length ==1 => id(0).text
    		 case None => throw new IllegalArgumentException("no id value for query in xml doc")
    		}
    val nameText = queryXML.\\("name").text
    val docIDText = queryXML.\\("docid").text
    val begText = queryXML.\\("beg").text
    val begInt = begText.toInt
    val endText = queryXML.\\("end").text
    val endInt = endText.toInt
    val entityTypeText = queryXML.\\("enttype").text
    val entityType = entityTypeText match{
      case "ORG" => ORG
      case "PER" => PER
      case _ => throw new IllegalArgumentException("improper 'enttype' value in xml doc")
    }
    val nodeIDText = queryXML.\\("nodeid").text
    val ignoreText = queryXML.\\("ignore").text
    val ignoreSlots = ignoreText.split(" ").toSet[String]
    
    
    //find slotsToFill by taking the difference between the global slots set
    // and the set specified in the xml doc
    val slotsToFill = entityType match{
      case ORG => {
        SlotTypes.organizationSlotTypes &~ ignoreSlots
      }
      case PER => {
        SlotTypes.personSlotTypes &~ ignoreSlots
      }
    }
    
    

    new KBPQuery(idText,nameText,docIDText,begInt,endInt,entityType,nodeIDText,slotsToFill)
  }
  
}