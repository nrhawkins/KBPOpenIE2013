package edu.knowitall.tac2013.pattern

import java.io.File

/**
 * Reads KbElements from the TAC 2009 knowledge base.
 */
class KnowledgeBaseReader(val path: File) extends Iterable[KbElement] {

  def iterator = path.listFiles.iterator.flatMap(file => KnowledgeBaseReader.readXml(file))
}

object KnowledgeBaseReader {
  
  import scala.xml.XML
  
  val wikiNameRegex = """(.*)(\s+\(.+\)\s*)""".r

  def readXml(file: File): Iterable[KbElement] = {

    val root = XML.loadFile(file)
    val entities = root.\("entity")
    entities.flatMap { entity =>
      val nameNode = entity.attribute("name").get.head
      val name = wikiNameRegex.findFirstIn(nameNode.text).get
      val id = entity.attribute("id").get.head.text
      val entityType = entity.attribute("type").get.head.text
      val factNodes = entity.attribute("facts").get
      val facts = factNodes map processFactNode
      facts map { fact =>
        val entityItem = KbItem(name, Some(id))
        val factItem = KbItem(fact.text, fact.linkId)
        val element = KbElement(entityItem, factItem, entityType, fact.factType)
        element
      }
    }
  }
  
  case class Fact(val factType: String, val text: String, val linkId: Option[String])
  
  def processFactNode(factNode: scala.xml.Node): Fact = {
    val factType = factNode.attribute("name").get.head.text
    // just consider one link node for now, one that's linked, if possible...
    val linkNodes = factNode.\("link")
    val linkNode = linkNodes.find(_.attribute("entity_id").isDefined) match {
      case Some(link) => Some(link)
      case None => linkNodes.headOption
    }
    linkNode match {
      case Some(link) => Fact(factType, link.text, link.attribute("entity_id").map(_.head.text))
      case None => Fact(factType, factNode.text, None)
    }
  }
  
  def main(args: Array[String]) {
    
    val path = args(0)
    
    new KnowledgeBaseReader(new File(path)) foreach println
  }
  
}