package edu.knowitall.tac2013.solr

import edu.knowitall.tac2013.openie.KbpExtraction
import org.apache.solr.common.SolrInputDocument
import org.apache.solr.client.solrj.SolrServer
import scopt.OptionParser
import edu.knowitall.srlie.SrlExtractor
import edu.knowitall.chunkedextractor.Relnoun
import edu.knowitall.tac2013.openie.ParsedKbpSentence
import edu.knowitall.tool.parse.graph.DependencyGraph

class KbpSolrPopulator(solrServer: SolrServer, val srl: SrlExtractor, val relnoun: Relnoun) extends SolrPopulator[KbpExtraction](solrServer) {
  
  private val tabRegex = "\t".r
  
  def extract(line: String): Iterable[KbpExtraction] = {
    
    val parsedKbpSentenceOpt = ParsedKbpSentence.read(line)
    parsedKbpSentenceOpt match {
      case Some(parsedKbpSentence) => extract(parsedKbpSentence)
      case None => {
        // Error reported by ParsedKbpSentence.read
        Iterable.empty
      }
    }
  }
  
  def extract(pks: ParsedKbpSentence): Iterable[KbpExtraction] = {
    
    val dgraph = DependencyGraph.deserialize(pks.dgraph)
    val srlExtrs = srl.apply(dgraph)
  }
  
  def toSolrInputDocument(e: KbpExtraction): SolrInputDocument = {
    val doc = new SolrInputDocument()
    doc.addField("arg1", e.arg1)
    doc.addField("rel", e.rel)
    doc.addField("arg2", e.arg2)
    doc.addField("arg1_postag", e.arg1postags)
    doc.addField("rel_postag", e.relpostags)
    doc.addField("arg2_postag", e.arg2postags)
    doc.addField("arg1_types", "") // add later?
    doc.addField("rel_types", "")
    doc.addField("arg2_types", "")
    doc.addField("context", "")
    doc.addField("confidence", e.confidence)
    doc.addField("sentence", e.sent)
    doc.addField("extractor", e.extractor)
    doc.addField("url", "")
    doc.addField("corpus", "tac2013")
    doc
  }
}

object KbpSolrPopulator {
  
  def main(args: Array[String]): Unit = {
    
    
  }
}


/*
<doc>
            <field name="id">{ extr.id }</field>

            <field name="arg1">{ extr.arg1 }</field>
            <field name="rel">{ extr.rel }</field>
            { extr.arg2s.map { arg2 =>
              <field name="arg2">{ arg2 }</field>
            }}

            <field name="arg1_postag">{ extr.sentence.tokens(extr.arg1Interval).map(_.postag).mkString(" ") }</field>
            <field name="rel_postag">{ extr.sentence.tokens(extr.relInterval).map(_.postag).mkString(" ") }</field>
            <field name="arg2_postag">{ extr.sentence.tokens(extr.arg2Interval).map(_.postag).mkString(" ") }</field>

            { extr.arg1Types(sentenceEntity.types).map { typ =>
              <field name="arg1_types">{ typ.descriptor }</field>
            }}
            { extr.relTypes(sentenceEntity.types).map { typ =>
              <field name="rel_types">{ typ.descriptor }</field>
            }}
            { extr.arg2Types(sentenceEntity.types).map { typ =>
              <field name="arg2_types">{ typ.descriptor }</field>
            }}

            <field name="context"></field>

            <field name="confidence">{ extr.confidence }</field>

            <field name="sentence">{ sentenceEntity.text }</field>

            <field name="extractor">{ extr.extractor }</field>
            <field name="url">{ sentenceEntity.document.path }</field>
            <field name="corpus">{ sentenceEntity.document.corpus }</field>
          </doc>
*/