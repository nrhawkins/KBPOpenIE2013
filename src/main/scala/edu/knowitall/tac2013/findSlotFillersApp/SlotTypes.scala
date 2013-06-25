package edu.knowitall.tac2013.findSlotFillersApp

import scala.io.Source

object SlotTypes {
  var personSlotTypes = List[String]()
  var organizationSlotTypes = List[String]()
  private val personSource = Source.fromURL(getClass.getResource("/edu/knowitall/tac2013/findSlotFillersApp/PersonSlotTypes.txt"))
  private val organizationSource = Source.fromURL(getClass.getResource("/edu/knowitall/tac2013/findSlotFillersApp/OrganizationSlotTypes.txt"))
  personSource.getLines.foreach(l => {
	  if (l.trim().contains("per:")){
	    personSlotTypes = l.trim() :: personSlotTypes
	  }
  })
  organizationSource.getLines.foreach(l => {
	  if (l.trim().contains("org:")){
	    organizationSlotTypes = l.trim() :: organizationSlotTypes
	  }
  })
}