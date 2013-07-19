package edu.knowitall.tac2013.app

import edu.knowitall.tac2013.solr.query.SolrHelper
import edu.knowitall.tac2013.stanford.annotator.utils.StanfordAnnotatorHelperMethods

object PhilipKaiser {
  
  def main(args: Array[String]){
    SolrHelper.setConfigurations("old", false)
    val rawDoc = SolrHelper.getRawDoc("NYT_ENG_20080101.0037.LDC2009T13")
    val altString = rawDoc.substring(4576,4589)
    val s = new StanfordAnnotatorHelperMethods()
    s.getCorefMentions(rawDoc,4576,4589);
    
  }

}