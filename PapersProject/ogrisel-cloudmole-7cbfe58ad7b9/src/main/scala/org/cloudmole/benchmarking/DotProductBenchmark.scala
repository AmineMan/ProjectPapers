package org.cloudmole.benchmarking

import org.cloudmole.util.Profiling._
import org.cloudmole.dataset.RandomDatasetGenerator
import it.unimi.dsi.fastutil.ints.Int2FloatOpenHashMap

/**
 * Simple benchmark tool to find out what is the best suited data structure to
 * do the job
 */
object DotProductBenchmark {

  def main(args: Array[String]) {
    val dimension = 200000
    val dataSetSize = 100

    println("generating dataset sparse version")
    val generator = RandomDatasetGenerator(dimension, 0.9, 0.1)
    val svData = (for ((x, y) <- generator.nextSamples(dataSetSize)) yield x).toList

    println("extracting dataset list version")
    val listData = (for (sv <- svData) yield sv.iterator.toList)

    val sparseVector = generator.model.parameters

    println("copying sparse parameters to dense buffer version")
    val denseVector = new Array[Float](generator.model.dimension)
    for (kv <- sparseVector)
      denseVector(kv._1) = kv._2

    def denseDotProduct(a: Array[Float], b: Iterable[(Int, Float)]): Float = {
      b.foldLeft(0f)((sum, kv) => sum + kv._2 * a(kv._1))
    }

    println("copying sparse parameters to Int2FloatOpenHashMap")
    val i2fOpenHashMap = new Int2FloatOpenHashMap(dimension)
    for (kv <- sparseVector)
      i2fOpenHashMap.put(kv._1, kv._2)
    i2fOpenHashMap.defaultReturnValue(0)

    def i2fDotProduct(a: Int2FloatOpenHashMap,  b: Iterable[(Int, Float)]): Float = {
      b.foldLeft(0f)((sum, kv) => sum + kv._2 * a.get(kv._1))
    }

    println("starting benchmark:")
    timed(printTime("sv times svData: ")){
        for (i <- 1 to 10; d <- svData) sparseVector dotproduct d
      }
    timed(printTime("sv times listData: ")){
        for (i <- 1 to 10; d <- listData) sparseVector dotproduct d
      }
//    timed(printTime("intMap times listData: ")){
//        for (d <- listData) mapDotProduct(intMap, d)
//      }
    timed(printTime("dv times listData: ")){
        for (i <- 1 to 10; d <- listData) denseDotProduct(denseVector, d)
      }

    timed(printTime("i2fOpenHashMap times listData: ")){
        for (i <- 1 to 10; d <- listData) i2fDotProduct(i2fOpenHashMap, d)
      }
  }

  
}
