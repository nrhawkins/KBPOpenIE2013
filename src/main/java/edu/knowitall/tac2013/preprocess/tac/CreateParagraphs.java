package edu.knowitall.tac2013.preprocess.tac;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

public class CreateParagraphs {

	static String dir = "/projects/pardosa/s5/raphaelh/tac";

	//static String in = dir + "/data/source_09nw";
	static String in = dir + "/data/source_10wb";
	static String out1 = dir + "/data/10wb/paragraphs.text";
	static String out2 = dir + "/data/10wb/paragraphs.meta";

	public static void main(String[] args) throws IOException {

		BufferedWriter w1 = new BufferedWriter(new OutputStreamWriter
				(new FileOutputStream(out1), "utf-8"));
		BufferedWriter w2 = new BufferedWriter(new OutputStreamWriter
				(new FileOutputStream(out2), "utf-8"));
		BufferedReader r = new BufferedReader(new InputStreamReader
				(new FileInputStream(in), "utf-8"));
		String l = null;
		int i = 0;
		while ((l = r.readLine())!= null) {
			String[] c = l.split("\t");
			if (c.length < 5) {System.out.println(l); continue; }
			w1.write(i + "\t" + c[2] + "\n");
			w2.write(i + "\t" + c[0] + "\t" + /*c[1] + "\t" +*/ c[3] + "\t" + c[4] + "\n");
			i++;
		}
		r.close();
		w1.close();
		w2.close();
	}

}
