package edu.knowitall.tac2013.prep

import scopt.OptionParser
import util.FileUtils
import java.io.File
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.common.Timing
import edu.knowitall.common.Resource

object OffsetSanityChecker {

  def main(args: Array[String]): Unit = {
    
    var annotationsFileName = "."
    var corpusPathName = "."
    
    val parser = new OptionParser() {
      arg("Annotation File", "Path to tac query annotation file.", { annotationsFileName = _ })
      arg("Corpus directory", "Path to tac corpus.", { corpusPathName = _ })
    }
      
    val annotationsFile = new File(annotationsFileName)
    val corpusPath = new File(corpusPathName)
    
    if (!parser.parse(args)) return
    else if (!annotationsFile.isFile || !corpusPath.isDirectory()) {
      System.err.println("Invalid annot. file or corpus path.")
      return
    } else {
      run(annotationsFile, corpusPath) 
    } 
  }
  
  def run(annotationsFile: File, corpusPath: File): Unit = {
    
    // load all of the files under corpusPath into a map by docId
    val getCorpusFile = loadCorpusFilesMap(corpusPath)
    
    var annotationsProcessed = 0
    var annotationsCorrect = 0
    
    // for each annotation, check it against its source doc according.
    Resource.using(io.Source.fromFile(annotationsFile, "UTF8")) { source =>

      for (annot <- source.getLines map Annotation.read) {
        val corpusFile = getCorpusFile.getOrElse(annot.docId, { // or else...
          throw new RuntimeException(s"No file found for docId=${annot.docId}")
        })
        if (verify(annot, corpusFile)) annotationsCorrect += 1
        annotationsProcessed += 1
        if (annotationsProcessed % 1000 == 0) 
          System.err.println(s"$annotationsProcessed annotations processed, $annotationsCorrect correct.")
      }
    }
  }
  
  def verify(annot: Annotation, sourceFile: File): Boolean = {
    
    // load the file as a big string
    val fileString = DocSplitterSpec.fileString(sourceFile.toURL)
    
    // verify filler
    val fillerLookup = fileString.substring(annot.finterval.start, annot.finterval.end)
    val fillerOk = fillerLookup.equals(annot.filler)
    
    val justificationLookup = fileString.substring(annot.jinterval.start, annot.jinterval.end)
    val justificationOk = justificationLookup.equals(annot.justification)
    
    if (!fillerOk) {
      System.err.println(
          s"Fill Mismatch in ${annot.docId}, found: $fillerLookup, exp: ${annot.filler} at ${annot.finterval.toString}")
    }
    if (!justificationOk) {
      System.err.println(
          s"Just Mismatch in ${annot.docId}, found: justificationLookup, exp: ${annot.justification} at ${annot.jinterval.toString}")
    }
    
    fillerOk && justificationOk
  }
  
  val dropExtensionRegex = "(.+)\\.([^\\.]+)".r
  
  def loadCorpusFilesMap(corpusPath: File): Map[String, File] = {
    
    var numFilesLoaded = 0
    System.err.print("Loading corpus filenames.")

    val (loadTimeNs, corpusFilesMap) = Timing.time {
      FileUtils.getFilesRecursive(corpusPath).map { file =>
        // Convert filename to docId by dropping extension.
        val docId = file.getName match {
          case dropExtensionRegex(fileName, extension) => fileName
          case _ => throw new RuntimeException(s"Could not get DocId from filename: ${file.getName}")
        }
        numFilesLoaded += 1
        if (numFilesLoaded % 2000 == 0) System.err.print(".")
        (docId, file)
      } toMap
    }
    System.err.println
    System.err.println(s"Loaded $numFilesLoaded files in ${Timing.Seconds.format(loadTimeNs)}.")
    return corpusFilesMap
  }
}

/**
 * An entry from the "annotation" file. Lines are tab separated and look like this:
 * filler_id	sf_id	system_id	slot_name	docid	fill_start_char	fill_end_char	filler	just_start_char	just_end_char	justification	norm_response	equiv_class_id	judgment
 * 609	SF_ENG_041	LDC	org:alternate_names	APW_ENG_20070319.1047.LDC2009T13	1579	1586	Carnival281	301	Carnival Cruise Lines	Carnival	438	1
 * 
 */
case class Annotation(val docId: String, val filler: String, val finterval: Interval, val justification: String, val jinterval: Interval)

object Annotation {
  /**
   * Read an annotation in tab-separated format from the TAC .tab file. 
   */
  def read(line: String): Annotation = {
    line.split("\t") match {
      case Array(fid, sfid, sid, sname, docid, fillStartChar, fillEndChar, filler, justStartChar, justEndChar, justification, _*) => {
        Annotation(
            docid,
            filler, 
            Interval.closed(justStartChar.toInt, justEndChar.toInt), 
            justification, 
            Interval.closed(justStartChar.toInt, justEndChar.toInt))
      }
      case _ => {
        throw new IllegalArgumentException("Too few fields in input line:\n\"%s\"".format(line))
      }
    }
  }
}


