package edu.knowitall.tac2013.stanford.annotator.utils;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.nio.CharBuffer;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.apache.commons.io.IOUtils;
import org.apache.commons.io.FileUtils;

import edu.knowitall.collection.immutable.Interval;
import edu.stanford.nlp.ling.CoreAnnotations.*;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.time.SUTimeMain;
import edu.stanford.nlp.time.TimeAnnotations.TimexAnnotation;
import edu.stanford.nlp.time.TimeAnnotations.TimexAnnotations;
import edu.stanford.nlp.time.SUTime;
import edu.stanford.nlp.time.Timex;
import edu.stanford.nlp.util.CoreMap;

import edu.knowitall.tac2013.solr.query.SolrHelper;



public class StanfordAnnotatorHelperMethods {
	
	private final StanfordCoreNLP suTimePipeline;
	private String filePath = "/homes/gws/jgilme1/docs/";
	
	public StanfordAnnotatorHelperMethods(){
		Properties suTimeProps = new Properties();
		suTimeProps.put("annotators", "tokenize, ssplit, pos, lemma, cleanxml, ner");
		suTimeProps.put("sutime.binders", "0");
		this.suTimePipeline = new StanfordCoreNLP(suTimeProps);

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
	
	private String normalizeTimex(String timexString){
		return timexString;
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
		    			if(tt != null){
		    				return normalizeTimex(tt.value());
		    			}
		    		}
		    	}
		    }
		}
	    
	    return originalString;
	}
}
