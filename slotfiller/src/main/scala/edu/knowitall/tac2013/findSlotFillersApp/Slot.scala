package edu.knowitall.tac2013.findSlotFillersApp

import scala.io.Source
import edu.knowitall.tac2013.findSlotFillersApp.KBPQueryEntityType._
import java.net.URL
import edu.knowitall.common.Resource

case class Slot(name: String, maxResults: Int, patterns: Seq[SlotPattern]) {
  require(name == name.trim)
}

object Slot {
  
  private def requireResource(urlString: String): URL = {
    val url = urlString.getClass.getResource(urlString)
    require(url != null, "Could not find resource: " + url)
    url
  }
  
  private val personResource = "/edu/knowitall/tac2013/findSlotFillersApp/PersonSlotTypes.txt"
  private val organizationResource = "/edu/knowitall/tac2013/findSlotFillersApp/OrganizationSlotTypes.txt"

  private def loadSlots(urlString: String, slotPrefix: String): Set[Slot] = {
    val url = requireResource(urlString)
    Resource.using(Source.fromURL(url)) { personSource =>
      val filterPrefix = personSource.getLines.filter(_.contains(slotPrefix))
      val lazySlots = filterPrefix map(_.trim) map fromName
      lazySlots.toList.toSet // how better to force non-lazy?
    }
  }
  
  def fromName(slotString: String): Slot = {
    
    val patterns = SlotPattern.patternsForSlotName(slotString)
    require(patterns.map(_.maxValues).distinct.size <= 1, s"Patterns for slot $slotString must have equal maxValues")
    val maxValues = patterns.headOption.flatMap(_.maxValues).getOrElse(0)
    Slot(slotString, maxValues, patterns)
  }
  
  
  val personSlots = loadSlots(personResource, "per:")

  val orgSlots = loadSlots(organizationResource, "org:")

  def getSlotTypesList(kbpQueryEntityType: KBPQueryEntityType) = {
    kbpQueryEntityType match {
      case ORG => personSlots
      case PER => orgSlots
    }
  }
}

