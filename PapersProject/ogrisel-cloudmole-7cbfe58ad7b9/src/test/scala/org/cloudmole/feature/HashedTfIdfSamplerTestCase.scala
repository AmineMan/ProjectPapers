package org.cloudmole.feature

import org.junit.Test;
import org.junit.Before;
import org.junit.Assert._;

class HashedTfIdfSamplerTestCase {

  val doc1 = """This is a test document with some random boring content."""

  val doc2 = """This is another document with an exclusive word."""

  val doc3 = """Some more not so random but still boring content."""

  val corpus = List(doc1, doc2, doc3)

  val sampler: HashedTfIdfSampler = new HashedTfIdfSampler(10000)
  
  @Before
  def setUp: Unit = {
    sampler.reset
    sampler sampleCorpus corpus
  }
  
  @Test
  def testSampleCorpus: Unit = {

    // sampler has been initialized in setup with 3 documents
    assertEquals(3L, sampler.sampledDocuments)
    
    // check some term counts in the total corpus
    def count(term: String) = {
      sampler.documentCounts(sampler.hasher.hashTerm(term))
    }
    assertEquals(2L, count("document"))
    assertEquals(1L, count("exclusive"))
    assertEquals(0L, count("missing"))
  }

  @Test
  def testTfIdfEstimate: Unit = {

    val tfidf = sampler.tfidf("missing exclusive document")
    def hash = sampler.hasher.hashTerm _
    
    // relatively common terms get a lower score than rarer documents:
    assertEquals(0.30543026f, tfidf(hash("document")))
    assertEquals(0.46209812f, tfidf(hash("exclusive")))
    
    // terms not occuring in the sample are estimated to appear
    // only in one document
    assertEquals(0.46209812f, tfidf(hash("missing")))

  }
  
}
