package edu.knowitall.tac2013.pattern

import java.io.File
import edu.knowitall.common.Resource.using

/**
 * Reads KbElements from the TAC 2009 knowledge base.
 */
class KnowledgeBaseReader(val path: File) extends Iterable[KbElement] {

  def iterator = path.listFiles.iterator.flatMap(file => KnowledgeBaseReader.readXml(file))
}

object KnowledgeBaseReader {
  
  import scala.xml.XML
  
  val wikiNameRegex = """(.*)(\s+\(.+\)\s*)?""".r

  def clean(str: String): String = str.replaceAll("\n", " ")

  def readXml(file: File): Iterator[KbElement] = {

    val root = XML.loadFile(file)
    val entities = root.\("entity")
    entities.iterator.flatMap { entity =>
      val nameNode = entity.attribute("name").get.head
      val name = nameNode.text match {
        case wikiNameRegex(realName, _) => realName
        case _ => throw new Exception("Should have been able to match: " + nameNode.text)
      }
      val id = entity.attribute("id").get.head.text
      val entityType = entity.attribute("type").get.head.text
      val factNodes = entity.\("facts").\("fact")
      val facts = factNodes map processFactNode
      facts.iterator map { fact =>
        val entityItem = KbItem(clean(name), Some(id))
        val factItem = KbItem(clean(fact.text), fact.linkId)
        val element = KbElement(entityItem, factItem, entityType, fact.factType)
        element
      } filter(e => e.entityType == "ORG" || e.entityType == "PER") filter(infoboxFilter)
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
  
  val infoboxResource = "/edu/knowitall/tac2013/pattern/top500infoboxes.txt" 
  def topInfoboxes = {
    val url = getClass.getResource(infoboxResource)
    require(url != null, "Could not find resource: " + infoboxResource)
    using(io.Source.fromURL(url, "UTF8")) { source => source.getLines.toSet }
  }
  
  def infoboxFilter(e: KbElement) = topInfoboxes.contains(e.slotname)
}

object InfoboxCounter extends App {
  val path = args(0)
  val out = new java.io.PrintStream(args(1))

  new KnowledgeBaseReader(new File(path)).groupBy(e => e.slotname).iterator.map({ case (slotname, es) => 
    (slotname, es.size) 
  }).toSeq.sortBy(-_._2).take(500) foreach { case (slotname, count) => out.println(slotname) }
}