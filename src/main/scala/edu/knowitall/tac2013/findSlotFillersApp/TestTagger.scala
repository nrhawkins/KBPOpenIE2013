package edu.knowitall.tac2013.findSlotFillersApp


import edu.knowitall.taggers.tag.TaggerCollection
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.stem.MorphaStemmer
import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.chunk.ChunkedToken
import edu.stanford.nlp.pipeline._
import java.util.Properties



object TestTagger extends App {

  
    val testSentence = "Microsoft was founded by Billionaire Paul Allen, in 1973."
      
	  val chunker = new OpenNlpChunker();
	  val morpha = new MorphaStemmer();
	  val taggers = TaggerCollection.fromPath("/homes/gws/jgilme1/knowitall/Taggers/xmlTaggerFiles/taggers")
	  
	  val chunkedSentence = chunker.chunk(testSentence)
	  var tokens = List[Lemmatized[ChunkedToken]]()
	  for (token <- chunkedSentence) {
	        val lemma = morpha.lemmatizeToken(token);
	        tokens = tokens ::: List(lemma)
	    }
	
	
	    val types = scala.collection.JavaConversions.asScalaIterable(taggers.tag(scala.collection.JavaConversions.asJavaList(tokens)))
	    
	    for ( t <- types){
	      println(t)
	    }
	    
	      
	  val props = new Properties()
	  props.put("annotators", "ner")
	  val p = new StanfordCoreNLP(props)
	  val doc = new Annotation(testSentence)
	  p.annotate(doc)
	  println(doc.toString())

      
}