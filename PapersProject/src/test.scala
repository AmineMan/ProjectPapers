package org.cloudmole.feature

//import ogrisel-cloudmole-7cbfe58ad7b9.src.main.scala.org.cloudmole.dataset.SparseVector

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.Math._

/**
 * Simple sparse counter that computes relative frequencies.
 * Counted items are identified with an integer index.
 */
class Counter extends HashMap[Int, Long] {
  
  var total: Long = 0
  
  override def default(key: Int): Long = 0
  
  def increment(item: Int): Unit = {
    this(item) += 1
    total += 1
  }
  
  // convert exact integer count in approximate floating point value
  def getCount(item: Int): Float = this(item)
  
  def getCountOrOne(item: Int): Float = {
    val count = this(item)
    if (count == 0L)
      1f
    else
      count
  }
  
  def frequency(item: Int): Float = {
    getCount(item) / total.toFloat
  }
  
}

/**
 * Compute approximate TF-log-IDF values of a corpus of documents
 * where terms are first hashed in a sparse feature space of
 * dimension hashSize expected to be smaller than MAXINT
 * 
 * HashedTfIdf should be first initialized by applying the countDocument
 * method to a statistically reprensatative sample of the corpus. Then for
 * each document, the tf-idf are to be computed with the tfidf method that
 * returns a SparseVector mapping term hash codes to tfidf of the term
 * relative to the document.
 * 
 * http://en.wikipedia.org/wiki/Tf-idf
 * http://hunch.net/~jl/projects/hash_reps/
 */
class HashedTfIdfSampler(hashSize: Int) {

  //val hasher = new FeatureHasher(hashSize)

  val documentCounts = new Counter
  
  var sampledDocuments: Long = 0
  
  // TODO: implement better tokenization (e.g. handle punctuation)
  def tokenize(document: String): Array[String] = document.split("""\s+""")

  /**
   * Update the current estimate of IDF using the terms of 'document'
   */
 // def sampleDocument(document: String): Unit = {
    // scan the unique occurences of terms in the document
    // and increment the document counter
   // tokenize(document).toSet[String].foreach(
     // term => documentCounts.increment(hasher hashTerm term))
    //sampledDocuments += 1
  //}

  //def sampleCorpus(documents: Iterable[String]): Unit = {
    //documents.foreach(sampleDocument)
  //}
  
  /**
   * Compute the TF-IDF estimate of the terms in the document
   */
  //def tfidf(document: String): SparseVector = {

    //val termCounts = new Counter
    //val hashedTerms = tokenize(document) map (hasher hashTerm _)

    // First pass count term occurrences inside current document (tf)
   // hashedTerms.foreach(termCounts.increment)
    

    // Second pass over distinct terms: compute the tf * idf product for each
    // word
   // def computeOne(sv: SparseVector, t: Int): SparseVector = {
     // val tf = termCounts frequency t
      // avoid division by zero by trading some accuracy
    //  val idf = sampledDocuments / documentCounts.getCountOrOne(t)
    //  sv(t) = tf * log(1 + idf).toFloat
   //   sv
    //}
   // hashedTerms.toSet[Int].foldLeft(new SparseVector(hashSize))(computeOne)
  //}

  //def reset: Unit = {
    //documentCounts.clear
   // sampledDocuments = 0
  //}

}