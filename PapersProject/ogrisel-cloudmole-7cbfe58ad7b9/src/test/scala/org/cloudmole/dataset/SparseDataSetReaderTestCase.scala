package org.cloudmole.dataset

import org.junit.Test;
import java.io.File
import java.io.FileOutputStream
import java.io.FileWriter
import java.io.StringWriter
import java.util.zip.GZIPOutputStream
import org.junit.Assert._;

class SparseDataSetReaderTestCase {

  val line1 = "1 3:-2.34534 5:0.54634 78:-0.23425"

  val line2 = "-1 0:-0.45564"

  val line3 = "1"

  val multiLine = List(line1, line2, line3).mkString("\n")

  @Test
  def testParseLine: Unit = {
    SparseDataSetReader.fromString(line1).next match {
      case (x: Iterable[(Int, Float)], y: Float) => {
          assertEquals(1f, y)
          assertEquals(3, x.size)
          val l = x.toList
          assertEquals((3, -2.34534f), l(0))
          assertEquals((5, 0.54634f), l(1))
          assertEquals((78,-0.23425f), l(2))
        }
      case null => fail("reader should not return null")
    }
    SparseDataSetReader.fromString(line2).next match {
      case (x: Iterable[(Int, Float)], y: Float) => {
          assertEquals(-1f, y)
          assertEquals(1, x.size)
          assertEquals((0, -0.45564f), x.toList(0))
        }
      case null => fail("reader should not return null")
    }
    SparseDataSetReader.fromString(line3).next match {
      case (x: Iterable[(Int, Float)], y: Float) => {
          assertEquals(1f, y)
          assertEquals(0, x.size)
        }
      case null => fail("reader should not return null")
    }
  }

  @Test
  def testParseMultiLine: Unit = {
    val reader = SparseDataSetReader.fromString(multiLine);
    assertTrue(reader.hasNext)

    reader.next match {
      case (x: Iterable[(Int, Float)], y: Float) => {
          assertEquals(1f, y)
          assertEquals(3, x.size)
          val l = x.toList
          assertEquals((3, -2.34534f), l(0))
          assertEquals((5, 0.54634f), l(1))
          assertEquals((78,-0.23425f), l(2))
        }
      case null => fail("reader should not return null")
    }

    assertTrue(reader.hasNext)

    reader.next match {
      case (x: Iterable[(Int, Float)], y: Float) => {
          assertEquals(-1f, y)
          assertEquals(1, x.size)
          assertEquals((0, -0.45564f), x.toList(0))
        }
      case null => fail("reader should not return null")
    }
    assertTrue(reader.hasNext)

    reader.next match {
      case (x: Iterable[(Int, Float)], y: Float) => {
          assertEquals(1f, y)
          assertEquals(0, x.size)
        }
      case null => fail("reader should not return null")
    }

    assertFalse(reader.hasNext)
  }

  def checkReset(reader: SparseDataSetReader): Unit = {
    // first pass on the multiLine dataset
    assertNotNull(reader.next)
    assertTrue(reader.hasNext)
    assertNotNull(reader.next)
    assertTrue(reader.hasNext)
    assertNotNull(reader.next)

    // end of the first pass over the dataset
    assertFalse(reader.hasNext)

    // let us reset the dataset
    var resetedReader = reader.reset
    assertNotNull(resetedReader.next)
    assertTrue(resetedReader.hasNext)
    assertNotNull(resetedReader.next)

    // reset again in the middle of the second pass
    resetedReader = resetedReader.reset
    resetedReader.drop(2)
    assertTrue(resetedReader.hasNext)
    assertNotNull(resetedReader.next)

    // end of the third pass over the dataset
    assertFalse(resetedReader.hasNext)
  }

  @Test
  def testResetString: Unit = {
    val reader = SparseDataSetReader.fromString(multiLine);
    checkReset(reader)
  }

  @Test
  def testResetSimpleFile: Unit = {
    val f = File.createTempFile("cloudmodle-testResetSimpleFile-", ".dataset")
    val writer = new FileWriter(f)
    writer.write(multiLine)
    writer.close()
    val reader = SparseDataSetReader.fromFile(f)
    checkReset(reader)
    reader.close
    f.delete
  }

  @Test
  def testResetGZIPFile: Unit = {
    val f = File.createTempFile("cloudmodle-testResetSimpleFile-", ".dataset.gz")
    val writer = new GZIPOutputStream(new FileOutputStream(f))
    writer.write(multiLine.getBytes())
    writer.close()
    val reader = SparseDataSetReader.fromGZIPFile(f)
    checkReset(reader)
    reader.close
    f.delete
  }

}
