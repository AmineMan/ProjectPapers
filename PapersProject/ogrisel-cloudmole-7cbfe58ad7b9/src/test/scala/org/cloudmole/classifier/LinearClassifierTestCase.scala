package org.cloudmole.classifier

import org.cloudmole.dataset.SparseVector
import org.cloudmole.dataset.RandomDatasetGenerator
import org.junit.Test
import org.junit.Before
import org.junit.Assert._

class LinearClassifierTestCase {

  var reference: LinearClassifier = null

  var generator: RandomDatasetGenerator = null

  var trainData: Iterable[(SparseVector, Float)] = null

  var testData: Iterable[(SparseVector, Float)] = null

  def assertBounded(min: Float, x: Float, max: Float): Unit =
    assertTrue("expected: %f <= %f <= %f" format (min, x, max),
               min <= x && x <= max)

  @Before // TODO: find a way to use BeforeClass instead
  def setUp: Unit  = {
    val dim = 10
    reference = new LinearClassifier(dim)
    reference.parameters ++ Map(0-> -.5f, 3 -> 0.4f, 5 -> -.1f, 9-> 0.42f)
    reference.bias = 0.2f
    generator = new RandomDatasetGenerator(reference, .8, 0, 42)
    testData = generator.nextSamples(5000)
    trainData = generator.nextSamples(1000)
  }

  @Test
  def testForward: Unit = {
    // some non null vectors
    val sv1 = SparseVector(10, 0-> -.5f, 3 -> 0.4f, 5 -> -.1f)
    val sv2 = SparseVector(10, 2-> 1.2f, 3 -> -1.f, 4 -> 0.1f, 5 -> 2.f)

    // create a linear module
    val lc = new LinearClassifier(10)

    // by default the module parameters are null hence the forward activation
    // is always zero
    assertEquals(0f, lc forward sv1)
    assertEquals(0f, lc forward sv2)
    assertEquals(List(0f, 0f), lc forward List(sv1, sv2))

    // if we set a positive bias, the the activation is always on:
    lc.bias = 0.01f
    assertEquals(1f, lc forward sv1)
    assertEquals(1f, lc forward sv2)
    assertEquals(List(1f, 1f), lc forward List(sv1, sv2))

    lc.bias = -0.01f
    assertEquals(-1f, lc forward sv1)
    assertEquals(-1f, lc forward sv2)
    assertEquals(List(-1f, -1f), lc forward List(sv1, sv2))

    // load the sv1 values as modle parameters
    lc.parameters ++ sv1
    assertEquals(lc.parameters, sv1)

    // the activation for sv1 is now on while sv2 is not positively correlated
    assertEquals(1f, lc forward sv1)
    assertEquals(-1f, lc forward sv2)
    assertEquals(List(1f, -1f), lc forward List(sv1, sv2))

    // change the sign of the module parameters and check that the module
    // is now anti correlated with sv1 and positively correlated with sv2
    lc.parameters *= -1
    assertEquals(-1f, lc forward sv1)
    assertEquals(1f, lc forward sv2)
    assertEquals(List(-1f, 1f), lc forward List(sv1, sv2))
  }

  @Test
  def testLearn: Unit = {
    // the reference model is accurate up to the noise level in the dataset
    assertBounded(0.99f, reference accuracy trainData, 1f)
    assertBounded(0.99f, reference accuracy testData, 1f)
    assertEquals(4f / reference.dimension, reference.parameters.density)

    // the zero lc model is not accurate at all since the null parameters and
    // biases make the model always predict 0f where only 1f or -1f are
    // expected
    val lc = new LinearClassifier(reference.dimension)
    assertEquals(0f, lc accuracy trainData)
    assertEquals(0f, lc accuracy testData)
    assertEquals(0f, lc.parameters.density)

    lc calibrate (trainData take 100)
    assertNotSame(0f, lc.eta0);
    assertNotSame(0L, lc.t);

    val dataIterator = trainData.iterator

    // train the model on a first batch of 10 samples
    lc train (dataIterator take 10)

    // accuracy is already much better
    assertBounded(0.83f, lc accuracy trainData, 0.85f)
    assertBounded(0.83f, lc accuracy testData, 0.85f)

    // the model is still completely dense
    assertEquals(1f, lc.parameters.density)

    // train the model on the following batch of 10 samples
    lc train (dataIterator take 10)

    // accuracy is already still improving
    assertBounded(0.87f, lc accuracy trainData, 0.89f)
    assertBounded(0.85f, lc accuracy testData, 0.87f)

    // the model is still completely dense
    assertEquals(1f, lc.parameters.density)

    // train the model on the following batch of 80 samples
    lc train (dataIterator take 80)

    // accuracy is already still improving
    assertBounded(0.90f, lc accuracy trainData, 0.92f)
    assertBounded(0.89f, lc accuracy testData, 0.91f)

    // at the 100th sample the regularizer kicks in and trim useless components
    assertEquals(0.7f, lc.parameters.density)

    // train the model on the remaining samples
    lc train dataIterator

    // accuracy is close to convergence
    assertBounded(0.97f, lc accuracy trainData, 0.99f)
    assertBounded(0.97f, lc accuracy testData, 0.99f)

    // the regularizer has almost successfully trimed all the useless
    // components
    assertEquals(0.7f, lc.parameters.density)

    // training another epoch reaches convergence an only keep the useful
    // components
    lc train trainData

    assertBounded(0.99f, lc accuracy trainData, 1f)
    assertBounded(0.99f, lc accuracy testData, 1f)
    assertEquals(0.4f, lc.parameters.density)
  }

}
