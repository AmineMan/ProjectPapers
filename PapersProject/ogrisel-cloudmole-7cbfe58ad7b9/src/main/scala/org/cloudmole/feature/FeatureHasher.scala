package org.cloudmole.feature

class FeatureHasher(size: Int) {
  
  def hashTerm(term: String): Int = term.hashCode % size
  
  def hashNGram(ngram: List[String]): Int = ngram.hashCode % size

}
