package edu.knowitall.tac2013.prep.util

import scala.io.Source
import java.io.File
import java.io.InputStream

/**
 * Some general helper methods pertaining to file i/o.
 */
object FileUtils {

  /**
   * If file is not a directory, return it
   * else, return all files (that are not directories) found recursively in this directory.
   */
  def getFilesRecursive(file: File): Iterator[File] = {
    if (!file.isDirectory()) Iterator(file)
    else (file.listFiles.iterator.flatMap { f => getFilesRecursive(f) })
  }

  /**
   *  Lazily flatten the lines from an iterator of sources while
   *  closing sources as they become empty.
   */
  def getLines(sources: Iterator[Source]): Iterator[String] = {
    val iter = new Iterator[Iterator[String]]() {
      def hasNext = sources.hasNext
      def next = new Iterator[String]() {
        val source = sources.next()
        var closed = false
        val lines = source.getLines
        def hasNext = { // lines.hasNext will fail if source is closed.
          if (closed)
            false
          else if (!lines.hasNext) {
            closed = true
            false
          } else
            true
        }
        def next() = {
          val line = lines.next
          if (!lines.hasNext) { source.close; closed = true }
          line
        }
      }
    }
    iter.flatten
  }
}

case class Line(val text: String, val terminator: String)

/**
 * A wrapper for io.Source that includes newlines.
 */
class LineReader private (source: Source) extends Iterator[Line] {
  
  private lazy val iter = source.buffered
 
  private val textBuffer = new StringBuilder
  private val termBuffer = new StringBuilder

  def getc() = iter.hasNext && {
    val ch = iter.next()
    if (ch == '\n') {
      termBuffer append ch
      false
    } else if (ch == '\r') {
      termBuffer append ch
      if (iter.hasNext && iter.head == '\n')
        termBuffer append iter.next()

      false
    } else {
      textBuffer append ch
      true
    }
  }
  def hasNext = iter.hasNext
  def next = {
    textBuffer.clear()
    termBuffer.clear()
    while (getc()) {}
    Line(textBuffer.toString, termBuffer.toString)
  }
  
  def close() = source.close()
}

object LineReader {
  
  def fromFile(file: File, charset: String): LineReader = {
    new LineReader(io.Source.fromFile(file, charset))
  }
  
  def fromInputStream(istream: InputStream, charset: String): LineReader = {
    new LineReader(io.Source.fromInputStream(istream, charset))
  }
}