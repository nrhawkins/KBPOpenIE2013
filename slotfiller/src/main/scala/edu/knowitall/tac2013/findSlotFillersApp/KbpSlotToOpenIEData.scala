package edu.knowitall.tac2013.findSlotFillersApp

// many of these variables can be empty like ""
class KbpSlotToOpenIEData(
  val kbpSlotName: String,
  val maxValues: Option[Int],
  val openIERelationString: Option[String],
  val arg2Begins: Option[String],
  val entityIn: Option[String],
  val slotFillIn: Option[String],
  val slotType: Option[String]) {

  def requireTrimmed(s: String) = require(s.equals(s.trim()))
  
  requireTrimmed(kbpSlotName)
  openIERelationString foreach requireTrimmed
  arg2Begins foreach requireTrimmed
  entityIn foreach requireTrimmed
  slotFillIn foreach requireTrimmed
  slotType foreach requireTrimmed
  
  def isValid(): Boolean = {
    if (openIERelationString.nonEmpty && maxValues.nonEmpty &&
      entityIn.nonEmpty && slotFillIn.nonEmpty) {
      true
    } else {
      false
    }
  }
}

object KbpSlotToOpenIEData {

  //instance instance of KbpSlotToOpenIEData instance with None type where the string
  // is empty
  def getKbpSlotToOpenIEDataInstance(KbpSlotNameArgString: String, MaxValuesArgString: String,
    OpenIERelationArgString: String, Arg2BeginsArgString: String, EntityInArgString: String,
    SlotFillInArgString: String, SlotTypeArgString: String): KbpSlotToOpenIEData = {

    val arg1 = KbpSlotNameArgString.trim()

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
      case _ => Some(OpenIERelationArgString.trim())
    }

    val arg4 = Arg2BeginsArgString match {
      case "" => None
      case _ => Some(Arg2BeginsArgString.trim())
    }

    val arg5 = EntityInArgString match {
      case "" => None
      case _ => Some(EntityInArgString.trim())
    }

    val arg6 = SlotFillInArgString match {
      case "" => None
      case _ => Some(SlotFillInArgString.trim())
    }

    val arg7 = SlotTypeArgString match {
      case "" => None
      case _ => Some(SlotTypeArgString.trim())
    }

    new KbpSlotToOpenIEData(arg1, arg2, arg3, arg4, arg5, arg6, arg7)

  }

}