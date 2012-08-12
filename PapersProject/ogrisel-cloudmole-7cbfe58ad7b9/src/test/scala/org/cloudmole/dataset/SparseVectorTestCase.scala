package org.cloudmole.dataset

import org.junit.Test;
import org.junit.Assert._;

class SparseVectorTestCase {

  @Test
  def testDensity: Unit = {
    // some non null vectors
    val sv1 = SparseVector(10, 0-> -.5f, 3 -> 0.4f, 5 -> -.1f)
    assertEquals(0.3f, sv1.density)

    val sv2 = SparseVector(100, 2-> 1.2f, 3 -> -1.f, 4 -> 0.1f, 5 -> 2.f)
    assertEquals(0.04f, sv2.density)

    val sv3 = new SparseVector ++ Map(1-> 1f, 99 -> 99f)
    assertEquals(100, sv3.dimension)
    assertEquals(0.02f, sv3.density)
  }

  @Test
  def testNullDotProduct: Unit = {

    val sv1 = new SparseVector(10)
    val sv2 = new SparseVector(10)
    assertEquals(0.f, sv1 dotproduct sv2)

    // add values to sv2
    sv2(2) = 0.f
    sv2(2342) = -0.2f
    sv2(32) = 0.5f

    // the product is still null since sv1 is null
    assertEquals(0.f, sv1 dotproduct sv2)
    assertEquals(0.f, sv2 dotproduct sv1)
  }

  @Test
  def testDotProduct: Unit = {
    // some non null vectors
    val sv1 = SparseVector(10, 0-> -.5f, 3 -> 0.4f, 5 -> -.1f)
    val sv2 = SparseVector(10, 2-> 1.2f, 3 -> -1.f, 4 -> 0.1f, 5 -> 2.f)
    assertEquals(-0.6f, sv1 dotproduct sv2)
  }

  @Test
  def testDefault: Unit = {

    // null vector dotproduct has all components implicitly set to 0
    val sv = new SparseVector(10)

    // the product is still null since sv1 is null
    assertEquals(0.f, sv(0))
    assertEquals(0.f, sv(10))
    assertEquals(0.f, sv(42))
  }

  @Test
  def testAddScale: Unit = {
    // some non null vectors, a scale and the expected sum
    val sv1 = SparseVector(10, 0-> -.5f, 3 -> 0.4f, 5 -> -.1f)
    val sv2 = SparseVector(10, 2-> 1.2f, 3 -> -1.f, 4 -> 0.1f, 5 -> 2.f)
    val scale = 0.1f
    val expected = SparseVector(10,
      0-> -.5f,
      2-> .12f,
      3 -> 0.3f,
      4 -> 0.01f,
      5 -> 0.1f)

    sv1.add(sv2, scale)
    assertTrue(expected.almostEqual(sv1, 0.00001f))
  }

}
