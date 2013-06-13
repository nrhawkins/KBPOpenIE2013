package edu.knowitall.tac2013.preprocess.tac;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.NodeList;
import org.w3c.dom.Document;

/**
 * Reads the raw XML document format from the KBP 2013 corpus and produces
 * flattened output suitable for input to later preprocessing steps.
 * 
 * @author rbart
 * 
 */
public class KbpCorpusParser {

	public static void main(String[] args) {
		
		String filename = args[0];
		
		List<String> content = getContent(filename);
		
		for (String str : content) {
			System.out.println(str);
		}
	}
	
	
	public static List<String> getContent(String filename) {
		List<String> lines = new ArrayList<String>();
		try {
			DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
			factory.setIgnoringElementContentWhitespace(true);
			// factory.setValidating(true);
			Document doc = factory.newDocumentBuilder().parse(new File(filename));

			// if it's a newswire article
			if (filename.contains("2009/nw")) {
				NodeList nodeList = doc.getElementsByTagName("P");
				for (int i = 0; i < nodeList.getLength(); i++) {
					lines.add(nodeList.item(i).getTextContent()
							.replace("\n", " ").trim());
				}
		    // if it's a web article
			} else if (filename.contains("2010/wb")) {
				NodeList nodeList = doc.getElementsByTagName("POST");

				for (int j = 0; j < nodeList.getLength(); ++j) {
					NodeList childNodeList = nodeList.item(j).getChildNodes();
					for (int i = childNodeList.getLength() - 1; i > -1; i--) {
						if (childNodeList.item(i).getNodeName().equals("#text")) {
							for (String line : childNodeList.item(i)
									.getTextContent().split("\n\n")) {
								lines.add(line.replace("\n", " ").trim());
							}
							break;
						}
					}
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return lines;
	}

}
