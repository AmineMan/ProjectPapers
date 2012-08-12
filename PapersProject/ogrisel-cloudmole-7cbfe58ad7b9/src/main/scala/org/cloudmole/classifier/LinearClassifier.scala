package org.cloudmole.classifier;

import org.apache.commons.logging.LogFactory

import org.cloudmole.dataset.SparseVector
import scala.Math._

object LinearClassifier {

  val log = LogFactory.getLog(LinearClassifier.getClass)
}

/**
 * Simple backpropagatable linear module with sparse vector of parameters
 */
class LinearClassifier(val dimension: Int) {

  //
  // Regularizer parameters
  //

  var trimRate: Float = 0.3f

  var maxNorm: Float = 10f

  var skip: Int = 100

  var count: Int = skip

  //
  // SGD parameters
  //

  var calibrationSize: Int = 1000

  var t: Long = 0

  var eta0: Float = 1e4f

  //
  // model parameters
  //

  var bias: Float = 0
  
  val parameters = new SparseVector(dimension)

  override def clone: LinearClassifier = {
    val lc  = new LinearClassifier(dimension)
    lc.trimRate = trimRate
    lc.maxNorm = maxNorm
    lc.skip = skip
    lc.count = count
    lc.eta0 = eta0
    lc.calibrationSize  = calibrationSize
    lc.t = t
    lc.parameters ++ parameters
    lc.bias = bias
    lc
  }

  def calibrate(dataset: Iterable[(Iterable[(Int, Float)], Float)]): (Float, Long) = {
    // reset count state
    count = skip
    var bestAccuracy = 0.0f
    val base = 10f
    for (i <- 3 to 5; j <- 0 to 3) {
      val lc = clone
      lc.eta0 = pow(base, i).toFloat
      lc.t = pow(base, i + j).toLong
      lc train dataset
      var currentAccuracy = lc accuracy dataset
      LinearClassifier.log info ("eta0=%f, t=%d, currentAccuracy=%f"
                                 format (lc.eta0, pow(base, i + j).toLong,
                                         currentAccuracy))
      if (currentAccuracy > bestAccuracy) {
        bestAccuracy = currentAccuracy
        eta0 = lc.eta0
        t = pow(base, i + j).toLong
      }
    }
    LinearClassifier.log info ("best choice eta0=%f, t=%d, currentAccuracy=%f"
                               format (eta0, t, bestAccuracy))
    (eta0, t)
  }

  def forward(input: Iterable[(Int, Float)]): Float = {
    signum((parameters dotproduct input) + bias)
  }

  def forward(input: Iterator[Iterable[(Int, Float)]]): Iterator[Float] = {
    input map forward
  }

  def forward(input: Iterable[Iterable[(Int, Float)]]): Iterable[Float] = {
    input map forward
  }

  /**
   * Train the model on a single example without regularization
   */
  def train(x: Iterable[(Int, Float)], y: Float, eta: Float): Unit = {
    val z = ((parameters dotproduct x) + bias) * y
    if (z < 1) {
      parameters.add(x, eta * y)
      bias += eta * y * 0.1f
    }
  }

  /**
   * Sparse pegasos regularizer: trim the relatively weak components and then
   * normalize the remaining to unit L2 norm using euclidian projection
   */
  def regularize(eta: Float): Unit = {

    // trim the components that have less that 1/trimRate the average abs
    val threshold = parameters.norm1 / parameters.size * trimRate
    parameters map (kv => if (abs(kv._2) < threshold) parameters remove kv._1)
    ()
    //
    //    // L2 projection to unit ball
    //    val norm = sqrt(parameters.valuesIterator.reduceLeft(
    //      (acc, x) => acc + pow(x, 2).toFloat) + pow(bias, 2)).toFloat
    //    if (norm > maxNorm) parameters /= norm.toFloat
  }

  /**
   * Train the model on a labeled dataset using SGD with pegasos
   * regularization
   */
  def train(dataset: Iterator[(Iterable[(Int, Float)], Float)]): Unit = {

    // fetch the first sample of the dataset in memory to able to reuse them
    // several times during both the calibration process and the regular
    // learning phase
    
    val firstSamples = (dataset take calibrationSize).toList
    if (t == 0) {
      // model in uninitilized, need to find eta0, t and bscale from data
      calibrate(firstSamples)
    }

    for (sample <- firstSamples.toStream ++ dataset) {
      // update the learning rate according to the 1 / t schedule
      val eta = eta0 / t

      // TODO: use a match expression for the following
      train(sample._1, sample._2, eta)

      // regularize the parameters every while
      count -= 1
      if (count <= 0) {
        regularize(eta)
        count = skip
      }
      t += 1
    }
  }

  /**
   * Train the model on a labeled dataset using SGD with pegasos
   * regularization
   */
  def train(dataset: Iterable[(Iterable[(Int, Float)], Float)]): Unit = {
    train(dataset.iterator)
  }

  /**
   * Test the model on a single example, returm true is the example is
   * correctly classified
   */
  def test(x: Iterable[(Int, Float)], y: Float): Boolean = forward(x) == y

  /**
   * Test the model on a single labeled sample, return 1 is the sample is
   * correctly classified, 0 otherwise
   */
  def test(labeledSample: (Iterable[(Int, Float)], Float)): Int =
    if (forward(labeledSample._1) == labeledSample._2) 1 else 0

  /**
   * Compute the accuracy (correct classification rate) on the given test set
   */
  def accuracy(dataset: Iterator[(Iterable[(Int, Float)], Float)]): Float = {
    val counts = dataset.foldLeft(0L, 0L)(
      (acc, sample) => (acc._1 + test(sample), acc._2 + 1))
    counts._1.toFloat / counts._2.toFloat
  }

  /**
   * Compute the accuracy (correct classification rate) on the given test set
   */
  def accuracy(dataset: Iterable[(Iterable[(Int, Float)], Float)]): Float = {
    accuracy(dataset.iterator)
  }
  
}
