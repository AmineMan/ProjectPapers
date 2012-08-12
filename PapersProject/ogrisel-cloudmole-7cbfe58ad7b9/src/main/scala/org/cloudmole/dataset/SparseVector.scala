package org.cloudmole.dataset

import it.unimi.dsi.fastutil.ints.Int2FloatOpenHashMap;
import scala.Math._

/**
 * Sparse vector with single precision floating point components
 * and dot product support
 */
@serializable
class SparseVector(var dimension: Int = 0) extends Int2FloatOpenHashMap {

  override def default(key : Int) = 0.f

  def apply(i: Int): Float = get(i)

  def update(i: Int, v: Float) = put(i, v)

  def dotproduct(that: SparseVector): Float = {
    if (that.size < size)
      that dotproduct this
    else {
      val iter = int2FloatEntrySet.fastIterator()
      var sum = 0.0f;
      while (iter.hasNext) {
        val e = iter.next;
        sum += e.getFloatValue * that.get(e.getIntKey);
      }
      return sum;
    }
  }

  def norm1: Float = {
    val iter = values.iterator
    var sum = 0.0f;
    while (iter.hasNext) {
      sum += abs(iter.nextFloat)
    }
    return sum;
  }

  def dotproduct(that: Iterable[(Int, Float)]): Float = {
    that.foldLeft(0f)((sum, kv) => sum + kv._2 * this.get(kv._1))
  }

  def add(that: Iterable[(Int, Float)], scale: Float): Unit = {
    that map (kv => this.put(kv._1, this.get(kv._1) + kv._2 * scale))
    ()
  }

  def almostEqual(that: SparseVector, tolerance: Float): Boolean = {
    if (size != that.size)
      false
    else {
      val iter = int2FloatEntrySet.fastIterator()
      var result = true
      while (iter.hasNext) {
        val e = iter.next
        if (abs(that(e.getIntKey) - e.getFloatValue) > tolerance)
          result = false
      }
      result
    }
  }

  def *=(s: Float): Unit = {
    if (s != 1) {
      val iter = int2FloatEntrySet.fastIterator()
      while (iter.hasNext) {
        val e = iter.next
        this.put(e.getIntKey, s * e.getFloatValue)
      }
    }
  }

  def /=(s: Float): Unit = {
    if (s != 1) {
      val iter = int2FloatEntrySet.fastIterator()
      while (iter.hasNext) {
        val e = iter.next
        this.put(e.getIntKey, s / e.getFloatValue)
      }
    }
  }
  
  def ++(keyvalues: Iterable[(Int, Float)]): SparseVector = {
    for (kv <- keyvalues) {
      this.put(kv._1, kv._2)
      dimension = dimension max (kv._1 + 1)
    }
    this
  }

  def ++(that: SparseVector): SparseVector = {
    dimension = dimension max that.dimension
    val iter = that.int2FloatEntrySet.fastIterator()
    while (iter.hasNext) {
      val e = iter.next
      this.put(e.getIntKey, e.getFloatValue)
    }
    this
  }

  def density: Float = size.toFloat / dimension

}

/**
 * Map style factory for the SparseVector class
 */
object SparseVector {

  def apply(dimension: Int, elems: (Int, Float)*) =
    new SparseVector(dimension) ++ elems

}
