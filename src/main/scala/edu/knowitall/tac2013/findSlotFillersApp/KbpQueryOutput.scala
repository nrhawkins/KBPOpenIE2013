package edu.knowitall.tac2013.findSlotFillersApp

import scala.io._
import java.io._
import edu.knowitall.tac2013.openie.KbpExtraction

object KbpQueryOutput {
  
  def printUnformattedOutput(listOfResults: List[(String,KbpSlotToOpenIEData,List[KbpExtraction])], filePath: String){
    
    val writer = new PrintWriter(new File(filePath))
    var printedKbpSlotNames = Array[String]()
    for(result <- listOfResults){
      
      val kbpSlotName = result._1
      val kbpOpenIEData = result._2
      //only return up to 20 solr Results
      val solrResultsArray = result._3.slice(0,20)
      
      if (!printedKbpSlotNames.contains(kbpSlotName)){
        writer.write("KBP SLOT NAME: " + kbpSlotName + "\n")
        printedKbpSlotNames = printedKbpSlotNames :+ kbpSlotName
      }
      
      writer.write("\tQuery Data:\t"+ "RelationTerms: "+kbpOpenIEData.openIERelationString.getOrElse({""})
          + "\t Arg2Begins: " + kbpOpenIEData.arg2Begins.getOrElse({""}) + "\t Entity In: " +
          kbpOpenIEData.entityIn.getOrElse({""}) + "\t SlotFill In: " + kbpOpenIEData.slotFillIn.getOrElse({""}) +
          "\t Slot type: " + kbpOpenIEData.slotType.getOrElse({""}) +"\n")
      
      writer.write("\tResults:\n")
      if (solrResultsArray.length ==0){
        writer.write("\t\tNil" + "\n")
      }
      
      
      for (solrResult <- solrResultsArray){
        
        writer.write("\t\targ1: " + solrResult.arg1.originalText + "\t rel: " + solrResult.rel.originalText + 
            "\t arg2: " + solrResult.arg2.originalText + "\t docID: " + solrResult.sentence.docId +
            "\t confidence: " + solrResult.confidence + "\t sentence: " + solrResult.sentence.chunks + "\n\n")


      }

      
    }
    
    writer.close()
    
  }

}