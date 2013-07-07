package edu.knowitall.tac2013.findSlotFillersApp

import edu.knowitall.common.Resource
import io.Source
import KBPQueryEntityType._

// many of these variables can be empty like ""
case class SlotPattern private (
  val slotName: String,
  val maxValues: Option[Int],
  val relString: Option[String],
  val arg2Begins: Option[String],
  val entityIn: Option[String],
  val slotFillIn: Option[String],
  val slotType: Option[String]) {

  import SlotPattern.requireTrimmed

  relString foreach requireTrimmed
  arg2Begins foreach requireTrimmed
  entityIn foreach requireTrimmed
  slotFillIn foreach requireTrimmed
  slotType foreach requireTrimmed

  val entityType: KBPQueryEntityType = if (slotName.startsWith("per:")) PER else ORG
  
  def isValid(): Boolean = {
    if (relString.nonEmpty && maxValues.nonEmpty &&
      entityIn.nonEmpty && slotFillIn.nonEmpty) {
      true
    } else {
      false
    }
  }
  
  def debugString = "rel: " + relString.getOrElse({ "" }) +
        "\t Arg2Begins: " + arg2Begins.getOrElse({ "" }) + "\t Entity In: " +
        entityIn.getOrElse({ "" }) + "\t SlotFill In: " + slotFillIn.getOrElse({ "" }) +
        "\t Slot type: " + slotType.getOrElse({ "" })
}

object SlotPattern {

  private def requireTrimmed(s: String) = require(s.equals(s.trim()), "String must be trimmed: \"$s\"".format(s))

  val personPatternResource = "/edu/knowitall/tac2013/findSlotFillersApp/KBP-OpenIE-Person.csv"
  val organizationPatternResource = "/edu/knowitall/tac2013/findSlotFillersApp/KBP-OpenIE-Organization.csv"
    
  def read(csvDataArray: Array[String]): Option[SlotPattern] = {
    
    csvDataArray match {
      case Array(slotName, maxValues, relString, arg2Begins, entityIn, slotFillIn, slotType, _*) => {
        val pattern = getSlotPattern(slotName, maxValues, relString, arg2Begins, entityIn, slotFillIn, slotType)
        Some(pattern)
      }
      case Array(slotName, maxValues, _*) => {
        val pattern = getSlotPattern(slotName, maxValues, "", "", "", "", "")
        Some(pattern)
      }
      case _ => {
        System.err.println("Couldn't parse pattern fields(%d): %s".format(csvDataArray.length, csvDataArray.mkString(", ")))
        None
      }
    }
  }

  //instance instance of KbpSlotToOpenIEData instance with None type where the string
  // is empty
  // requires that input strings are trimmed.
  private def getSlotPattern(
    slot: String,
    MaxValuesArgString: String,
    OpenIERelationArgString: String,
    Arg2BeginsArgString: String,
    EntityInArgString: String,
    SlotFillInArgString: String,
    SlotTypeArgString: String): SlotPattern = {
    
    requireTrimmed(MaxValuesArgString)
    requireTrimmed(OpenIERelationArgString)
    requireTrimmed(Arg2BeginsArgString)
    requireTrimmed(EntityInArgString)
    requireTrimmed(SlotFillInArgString)
    requireTrimmed(SlotTypeArgString)

    //determine if this is a valid integer
    val arg2 = {
      try {
        Some(MaxValuesArgString.toInt)
      } catch {
        case e: NumberFormatException => { None }
      }
    }

    val arg3 = OpenIERelationArgString match {
      case "" => None
      case _ => Some(OpenIERelationArgString)
    }

    val arg4 = Arg2BeginsArgString match {
      case "" => None
      case _ => Some(Arg2BeginsArgString)
    }

    val arg5 = EntityInArgString match {
      case "" => None
      case _ => Some(EntityInArgString)
    }

    val arg6 = SlotFillInArgString match {
      case "" => None
      case _ => Some(SlotFillInArgString)
    }

    val arg7 = SlotTypeArgString match {
      case "" => None
      case _ => Some(SlotTypeArgString)
    }

    new SlotPattern(slot, arg2, arg3, arg4, arg5, arg6, arg7)
  }
}