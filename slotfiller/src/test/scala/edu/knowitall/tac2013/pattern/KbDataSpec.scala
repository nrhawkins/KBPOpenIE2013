package edu.knowitall.tac2013.pattern

import org.scalatest._

class KbDataSpec extends FlatSpec {

  "A KbQuery" should "produce the right query string" in {
    
    val entity = KbItem("Ben Bernanke", Some("E0780408"))
    val fill = KbItem("SomeSlotFill", None)
    
    val element = KbElement(entity, fill, "PER", "per:some_slot")
    
    val query = KbQuery(element, entityArg1 = true)
    
    val expectedQueryString = "(arg1Text:\"Ben Bernanke\" OR arg1WikiLinkNodeId:\"E0780408\") AND (arg2Text:\"SomeSlotFill\")"
    
    assert(query.queryString === expectedQueryString)
  }
}