package org.cloudmole.dataset

import java.io.ByteArrayInputStream
import java.io.Closeable
import java.io.File
import java.io.FileInputStream
import java.io.InputStream
import java.net.URL
import java.util.zip.GZIPInputStream
import org.apache.commons.logging.LogFactory
import scala.io.Codec
import scala.io.Source

/**
 * Wrapper to parse and input stream that holds data that hold libsvm
 * formatted classification data.
 */
class SparseDataSetReader(protected var source: Source)
extends Iterator[(Iterable[(Int, Float)], Float)] with Closeable {

  protected var lines: Iterator[String] = source.getLines()

  def hasNext: Boolean = lines.hasNext

  def next: (Iterable[(Int, Float)], Float) = {
    // split the label from the sparse vector representation
    val parts = lines.next.split(" ", 2)

    // the first element of the line is the label of the sample
    val y = parts(0).toFloat

    // parse the "index:value index:value ..." pattern and load it in a new
    // SparseVector instance
    if (parts.length > 1) {
      val x = parts(1).split(" ").map(_.split(":")).map(
        z => (z(0).toInt, z(1).toFloat))
      (x, y)
    } else {
      (Nil, y)
    }
    
  }

  override def drop(n: Int): SparseDataSetReader = {
    lines.drop(n)
    this
  }
  
  override def close: Unit = source.close
  
  def reset: SparseDataSetReader = {
    source = source.reset
    lines = source.getLines()
    this
  }

}

/**
 * Helpers to instanciate SparseDataSetReader instances from various sources.
 */
object SparseDataSetReader {

  val log = LogFactory.getLog(SparseDataSetReader.getClass)

  def fromString(s: String): SparseDataSetReader = {
    new SparseDataSetReader(Source.fromString(s))
  }

  def fromPath(filepath: String)
  (implicit codec: Codec = Codec.default): SparseDataSetReader = {
    new SparseDataSetReader(Source.fromPath(filepath)(codec))
  }

  def fromFile(file: File, bufferSize: Int = Source.DefaultBufSize)
  (implicit codec: Codec = Codec.default): SparseDataSetReader = {
    new SparseDataSetReader(Source.fromFile(file, bufferSize)(codec))
  }

  def fromURL(url: URL)
  (implicit codec: Codec = Codec.default): SparseDataSetReader = {
    new SparseDataSetReader(Source.fromURL(url)(codec))
  }

  def fromGZIPPath(filepath: String, bufferSize: Int = Source.DefaultBufSize)
  (implicit codec: Codec = Codec.default): SparseDataSetReader = {
    val f = new File(filepath)
    fromGZIPFile(f, bufferSize)(codec)
  }
  
  def fromGZIPFile(file: File, bufferSize: Int = Source.DefaultBufSize)
  (implicit codec: Codec = Codec.default): SparseDataSetReader = {

    val inputStream = new GZIPInputStream(new FileInputStream(file))

    fromInputStream(
      inputStream,
        bufferSize,
        () => {inputStream.close(); fromGZIPFile(file, bufferSize)(codec)},
        () => inputStream.close()
    )(codec)

  }

  def fromGZIPURL(url: URL, bufferSize: Int = Source.DefaultBufSize)
  (implicit codec: Codec = Codec.default): SparseDataSetReader = {

    val inputStream = new GZIPInputStream(url.openStream())
    
    fromInputStream(
      inputStream,
        bufferSize,
        () => {inputStream.close(); fromGZIPURL(url, bufferSize)(codec)},
        () => inputStream.close()
    )(codec)

  }

  def fromInputStream(inputStream: InputStream,
                      bufferSize: Int = Source.DefaultBufSize,
                      reset: () => SparseDataSetReader = null,
                      closeFn: () => Unit = null)
  (implicit codec: Codec = Codec.default): SparseDataSetReader = {
    // workaround for default arguments being unable to refer to other parameters
    val resetFn = {
      if (reset == null)
        () => fromInputStream(inputStream, bufferSize, reset, closeFn)
      else
        reset
    }
    new SparseDataSetReader(Source.fromInputStream(inputStream, bufferSize)
                            (codec)) {
      override def reset: SparseDataSetReader = resetFn()
      override def close: Unit = closeFn()
    }
  }

}
