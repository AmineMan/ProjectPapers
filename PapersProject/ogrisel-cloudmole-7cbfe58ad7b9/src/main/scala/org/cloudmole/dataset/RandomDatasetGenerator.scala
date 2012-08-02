package org.cloudmole.dataset

import org.cloudmole.classifier.LinearClassifier
import scala.util.Random
import scala.Math._

/**
 * Helper class to generate training data  according to a predifined linear
 * model potentially with label flipped with probability noise
 */
class RandomDatasetGenerator(val model: LinearClassifier, val density: Double,
                             val noise: Double = 0, val seed: Int = 0) {

  val prng = new Random(seed)

  val components = (density * model.dimension).toInt

  val a = sqrt(3f / components).toFloat

  /**
   * Generate a new random vector of fixed density and expected unit norm along
   * with a binary label corrupted with probability noise
   */
  def nextSample: (SparseVector, Float) = {
    // TODO refactor me to generate List[(Int, Float)] instead an spare the
    // hash computation and temporary object allocations of the hash map
    // implementations
    val x = RandomDatasetGenerator.randomVector(
      model.dimension, components, a, prng)
    val y = model forward x
    if (prng.nextFloat > noise)
      (x, y)
    else
      (x, -y)
  }

  def nextSamples(n: Int): Iterable[(SparseVector, Float)] = {
    (for (i <- 1 to n) yield nextSample).toList
  }

}

object RandomDatasetGenerator {

  /**
   * Build a dataset generator with a random reference model
   */
  def apply(dimension: Int, referenceDensity: Double, density: Double,
            noise: Double = 0, seed: Int = 0): RandomDatasetGenerator = {
    val prng = new Random(seed)
    val model = randomClassifier(dimension, referenceDensity, prng)
    new RandomDatasetGenerator(model, density, noise, prng.nextInt)
  }

  def randomClassifier(dimension: Int, density: Double, prng: Random = null)
  :LinearClassifier = {
    val rng = if (prng != null) prng else new Random
    val model = new LinearClassifier(dimension)
    val nonZerocomponents = (density * dimension).toInt
    val width = sqrt(3f / nonZerocomponents).toFloat
    model.parameters ++ randomVector(dimension, nonZerocomponents, width, rng)
    model.bias = (rng.nextFloat * 2 - 1) * width
    model
  }

  def randomVector(dimension: Int, components: Int, width: Float,
                   prng: Random = null)
  : SparseVector = {
    val rng = if (prng != null) prng else new Random
    val v = new SparseVector(dimension)
    for (i <- 1 to components) {
      v(abs(rng.nextInt) % dimension) = (rng.nextFloat * 2 - 1) * width
    }
    v
  }
}
