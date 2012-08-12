package org.cloudmole.benchmarking

import org.cloudmole.util.Profiling._
import org.cloudmole.dataset.SparseDataSetReader
import org.cloudmole.classifier.LinearClassifier
import scala.io.Source

/**
 * Simple benchmark utility to time one pass training on the RCV1 dataset with
 * GZIP compression
 *
 * The dataset can be found on Antoine Bordes website:
 *   wget -c http://webia.lip6.fr/~bordes/datasets/binary/rcv1.tar.gz
 *   tar -zxvf rcv1.tar.gz
 */
object RCV1Benchmark {

  val defaultGZipPath = "/home/ogrisel/data/rcv1/train.dat.gz"

  val defaultPath = "/home/ogrisel/data/rcv1/train.dat"

  val dimension = 16000
  
  val scanSize = 10000

  val bufferSize = Source.DefaultBufSize * 10

  def main(args: Array[String]) {

    val gzipPath = if (args.size > 0) args(0) else defaultGZipPath
    val path = if (args.size > 1) args(1) else defaultPath

    val reader = SparseDataSetReader.fromGZIPPath(gzipPath)
    println("benchmarking from GZIP source default buffer size")
    benchWihReader(reader)

    val reader2 = SparseDataSetReader.fromGZIPPath(gzipPath, bufferSize)
    println("benchmarking from GZIP source")
    benchWihReader(reader2)

    val reader3 = SparseDataSetReader.fromPath(path)
    println("benchmarking from decompressed source")
    benchWihReader(reader3)

  }

  def benchWihReader(reader: SparseDataSetReader): Unit = {
    timed(printTime("scanning the %d first samples of the RCV1: "
                    format scanSize)) {
      reader.reset.drop(scanSize)
    }

    timed(printTime("reading the %d first samples of the RCV1: "
                    format scanSize)) {
      (reader.reset take scanSize).foldLeft(0)((acc, sample) => acc)
    }

    val lc = new LinearClassifier(dimension)
    timed(printTime("training model on the %d first samples of the RCV1: "
                    format scanSize)) {
      lc train (reader.reset take scanSize)
    }

  }
}