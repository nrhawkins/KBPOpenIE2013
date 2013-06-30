package edu.knowitall.tac2013.prep

import scopt.OptionParser
import util.FileUtils
import java.io.File
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.common.Timing
import edu.knowitall.common.Resource

object OffsetSanityChecker {

  def main(args: Array[String]): Unit = {
    
    var annotationsFileName = ""
    var corpusPathName = ""
    
    val parser = new OptionParser() {
      arg("Annotation File", "Path to tac query annotation file.", { str => annotationsFileName = str })
      arg("Corpus directory", "Path to tac corpus.", { str => corpusPathName = str })
    }
      
    if (!parser.parse(args)) 
      return
    else {
      println(s"Running with:\nAnnotations at: $annotationsFileName\nCorpus at:$corpusPathName")
      val annotationsFile = new File(annotationsFileName)
      val corpusPath = new File(corpusPathName)
      run(annotationsFile, corpusPath) 
    } 
  }
  
  def run(annotationsFile: File, corpusPath: File): Unit = {
   
    val annotations = Resource.using(io.Source.fromFile(annotationsFile, "UTF8")) { source =>
      source.getLines.drop(1) map Annotation.read toList
    }
    // load all of the files under corpusPath into a map by docId
    val getCorpusFile = loadCorpusFilesMap(annotations, corpusPath)
    
    var annotationsProcessed = 0
    var annotationsCorrect = 0

    // for each annotation, check it against its source doc according.
    for (annot <- annotations) {
      val corpusFile = getCorpusFile.getOrElse(annot.docId, { // or else...
        throw new RuntimeException(s"No file found for docId=${annot.docId}")
      })
      if (verify(annot, corpusFile)) annotationsCorrect += 1
      annotationsProcessed += 1
      if (annotationsProcessed % 100 == 0)
        println(s"$annotationsProcessed annotations processed, $annotationsCorrect correct.")
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
      println(
          s"Fill Mismatch in ${annot.docId}, found: $fillerLookup, exp: ${annot.filler} at ${annot.finterval.toString}")
    }
    if (!justificationOk) {
      println(
          s"Just Mismatch in ${annot.docId}, found: justificationLookup, exp: ${annot.justification} at ${annot.jinterval.toString}")
    }
    
    fillerOk && justificationOk
  }
  
  val dropExtensionRegex = "(.+)\\.([^\\.]+)".r
  
  def loadCorpusFilesMap(annotations: Seq[Annotation], corpusPath: File): Map[String, File] = {
    
    var numFilesLoaded = 0
    var numFilesSkipped = 0
    
    def docIds = annotations.map(_.docId).toSet

    val (loadTimeNs, corpusFilesMap) = Timing.time {
      val allFiles = FileUtils.getFilesRecursive(corpusPath)
      val docIdFiles = allFiles.flatMap { file =>
        // Convert filename to docId by dropping extension.
        file.getName match {
          case dropExtensionRegex(docId, extension) => {
            numFilesLoaded += 1
            if (numFilesLoaded % 10000 == 0) {
              println(s"$numFilesLoaded files loaded, $numFilesSkipped files skipped.")
            }
            Some((docId, file))
          }
          case _ => {
            numFilesSkipped += 1
            None
          }
        }
      }
      val filesOfInterest = docIdFiles filter { case (docId, file) => docIds.contains(docId) }
      filesOfInterest toMap
    }
    println
    println(s"Scanned $numFilesLoaded files, skipped $numFilesSkipped, retained ${corpusFilesMap.size} in ${Timing.Seconds.format(loadTimeNs)}.")
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


