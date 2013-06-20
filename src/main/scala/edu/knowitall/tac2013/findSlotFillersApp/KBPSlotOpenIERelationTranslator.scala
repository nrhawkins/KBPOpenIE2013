
package edu.knowitall.tac2013.findSlotFillersApp
import scala.io.Source
import scala.collection.mutable.Map

object KBPSlotOpenIERelationTranslator {
  
  def getOrganizationMap(): Map[String,String] = {
    val source = Source.fromURL(getClass.getResource("/KBP-OpenIE.csv"))
    val lines = source.getLines().toList
    var KBP_OpenIEMap = Map[String,String]()
    for( line <- lines){
      val splitLine = line.split(",")
      KBP_OpenIEMap += (splitLine(0) -> splitLine(1))
    }
    KBP_OpenIEMap
  }
    
  
  def getPersonMap(): Map[String,String] = {
    
    Map[String,String]()
  }

}