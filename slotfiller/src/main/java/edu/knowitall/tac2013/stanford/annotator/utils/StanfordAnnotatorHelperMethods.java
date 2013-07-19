package edu.knowitall.tac2013.stanford.annotator.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.regex.Pattern;

import org.apache.commons.io.IOUtils;

import edu.knowitall.collection.immutable.Interval;
import edu.stanford.nlp.dcoref.CorefChain;
import edu.stanford.nlp.dcoref.CorefChain.CorefMention;
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefChainAnnotation;
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefClusterIdAnnotation;
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefGraphAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.*;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.time.TimeAnnotations.TimexAnnotation;
import edu.stanford.nlp.time.Timex;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.util.IntTuple;
import edu.stanford.nlp.util.Pair;

import edu.knowitall.tac2013.app.Candidate;
import edu.knowitall.tac2013.app.KBPQueryEntityType;
import edu.knowitall.tac2013.solr.query.SolrHelper;



public class StanfordAnnotatorHelperMethods {
	
	private final StanfordCoreNLP suTimePipeline;
	private final StanfordCoreNLP corefPipeline;
	private String filePath = "/homes/gws/jgilme1/docs/";
	
	public StanfordAnnotatorHelperMethods(){
		Properties suTimeProps = new Properties();
		suTimeProps.put("annotators", "tokenize, ssplit, pos, lemma, cleanxml, ner");
		suTimeProps.put("sutime.binders", "0");
		suTimeProps.put("clean.datetags","datetime|date|dateline");
		this.suTimePipeline = new StanfordCoreNLP(suTimeProps);
		
		Properties corefProps = new Properties();
	    corefProps.put("annotators", "tokenize, cleanxml, ssplit, pos, lemma, ner, parse, dcoref");
	    //clean all xml tags
		this.corefPipeline = new StanfordCoreNLP(corefProps);

	}
	
	public static void main(String[] args) throws FileNotFoundException, IOException{
		StanfordAnnotatorHelperMethods sh = new StanfordAnnotatorHelperMethods();
		sh.runSuTime("testXMLDoc");
		
	}
	
	public void runSuTime(String docID) throws FileNotFoundException, IOException{
		String filePathPlusDocId = this.filePath+docID;
		FileInputStream in = new FileInputStream(new File(filePathPlusDocId));
		String fileString = IOUtils.toString(in,"UTF-8");
		in.close();
		
		Annotation document = new Annotation(fileString);
		suTimePipeline.annotate(document);
		
		List<CoreMap> sentences = document.get(SentencesAnnotation.class);
	    for(CoreMap sentence: sentences){
	    	for(CoreLabel token: sentence.get(TokensAnnotation.class)){
	    		String word = token.get(TextAnnotation.class);
	    		String ne = token.get(NamedEntityTagAnnotation.class);
	    		String net = token.get(NormalizedNamedEntityTagAnnotation.class);
	    		Timex tt = token.get(TimexAnnotation.class);
	    		String tts = "";
	    		if(tt != null){
	    			tts = tt.value();
	    		}
	    		System.out.println(word+ " " + ne + " " + net + " " + tts);
	    	}
	    }
	    
	    String s =document.get(NamedEntityTagAnnotation.class);
	    System.out.println(s);

	}
	
	private String normalizeTimex(Timex t){
		if(t.timexType() == "DATE"){
	      String timexString = t.value();
	      if (timexString == null) return "";
	      String formattedString = timexString;
	      if(Pattern.matches("\\w{4}", timexString)){
	    	  formattedString = timexString +"-XX-XX";
	      }
	      else if(Pattern.matches("\\w{2}-\\w{2}",timexString)){
	    	  formattedString = "XXXX-" + timexString; 
	      }
	      else if(Pattern.matches("\\w{4}-\\w{2}", timexString)){
	    	  formattedString = timexString + "-XX";
	      }
		  return formattedString;
		}
		else{
			return "";
		}
	}
	

	
	public String getNormalizedDate(Interval charInterval, String docId, String originalString) throws IOException{
		String xmlDoc = SolrHelper.getRawDoc(docId);
		if(xmlDoc.trim().isEmpty()){
			return originalString;
		}
		else{
			Annotation document = new Annotation(xmlDoc);
			suTimePipeline.annotate(document);
	
			List<CoreMap> sentences = document.get(SentencesAnnotation.class);
		    for(CoreMap sentence: sentences){
		    	for(CoreLabel token: sentence.get(TokensAnnotation.class)){
		    		Timex tt = token.get(TimexAnnotation.class);
		    		if(charInterval.intersects(Interval.closed(token.beginPosition(), token.endPosition()))){
		    			if(tt != null && tt.value() != null){
		    				return normalizeTimex(tt);
		    			}
		    		}
		    	}
		    }
		}
	    
	    return originalString;
	}
	
	public List<CorefMention> getCorefMentions(String xmlString, Interval interval) {
		Annotation document = new Annotation(xmlString);
		corefPipeline.annotate(document);
		
		
		
		Map<Integer, CorefChain> graph = document.get(CorefChainAnnotation.class);
		for(Integer i : graph.keySet()){
			System.out.println("GROUP " + i);
			CorefChain x = graph.get(i);
			for( CorefMention m : x.getMentionsInTextualOrder()){
				System.out.println(m.mentionSpan);
			}
		}

		Integer corefClusterID = null;
		
		List<CoreMap> sentences = document.get(SentencesAnnotation.class);
		for(CoreMap sentence : sentences){
			for(CoreLabel token : sentence.get(TokensAnnotation.class)){
				
			}
		}
		List<Pair<IntTuple,IntTuple>> x  = document.get(CorefGraphAnnotation.class);

		
	    for(CoreMap sentence: sentences){
	    	for(CoreLabel token: sentence.get(TokensAnnotation.class)){
	    		if(token.beginPosition() == interval.start()){
	    			corefClusterID = token.get(CorefClusterIdAnnotation.class);
	    		}
	    	}
	    }
	    
		
	    if(corefClusterID != null){
	    	return graph.get(corefClusterID).getMentionsInTextualOrder();
	    }
	    else{
	    	return new ArrayList<CorefMention>();
	    }
		
	}
	
	public Interval getAlternateSlotMentions(String xmlString, Interval interval, Candidate candidate){
		Annotation document = new Annotation(xmlString);
		corefPipeline.annotate(document);
		
		Map<Integer, CorefChain> graph = document.get(CorefChainAnnotation.class);
		Integer corefClusterID = null;
		List<CoreMap> sentences = document.get(SentencesAnnotation.class);
	    for(CoreMap sentence: sentences){
	    	for(CoreLabel token: sentence.get(TokensAnnotation.class)){
	    		if(token.beginPosition() == interval.start()){
	    			corefClusterID = token.get(CorefClusterIdAnnotation.class);
	    		}
	    	}
	    }
	    
	    if(corefClusterID != null){
	    	List<CorefMention> filteredCorefMentions = new ArrayList<CorefMention>();
	    	List<CorefMention> corefMentions = graph.get(corefClusterID).getMentionsInTextualOrder();
	    	
	    	for(CorefMention x : corefMentions){
	    		
	    	}
	    }
	    else{
	    }
	    return Interval.open(0, 1);
		
	}
	
	/**
	 * Provides a lookup method for taking corefMentions and finding their NER tagged
	 * substrings.
	 * @param annotatedDocument
	 * @param position
	 * @return
	 */
	private Interval getNamedEntityAtPosition(Annotation annotatedDocument, IntTuple position, KBPQueryEntityType entityType){
		
		return Interval.open(0, 1);
	}
	
	
	private CoreLabel getTokenAtInterval(Annotation annotatedDocument, Interval interval){
		
		return null;
	}
}
