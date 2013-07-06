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

  private def loadSlots(urlString: String, slotPrefix: String): Set[String] = {
    val url = requireResource(urlString)
    Resource.using(Source.fromURL(url)) { personSource =>
      val filterPrefix = personSource.getLines.filter(_.contains(slotPrefix))
      filterPrefix.map(_.trim).toList.toSet // how better to force non-lazy?
    }
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

