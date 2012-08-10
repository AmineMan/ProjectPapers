package paper

import java.io._
import java.util.Scanner
import net.sf.classifier4J._


import math._
import java.util.ArrayList
import java.io.{Closeable, File, FileWriter, PrintWriter}
import scala.List
import collection.mutable.ArrayBuffer
import scala.swing.model.Matrix

object bagOfWords {

// Code has to be made generic for any text file parsed and for the whole dataset to be accurate
	  def main(args : Array[String]): Unit = {
	    
// we compute the array of scores for the vectors of words for every document
 val tfidfArray = new Array[Array[Double]](dictionnary.length,datasetSize)
 
  for (i <- 0 to dictionnary.length -1){
    println(i)
    for (j <- 0 to datasetSize -1){
      tfidfArray(i)(j) = tfidf(dictionnary(i),j)
      println(tfidfArray(i)(j))
    }
  }
   
  //once we have the scores we can compute the absolute distance between papers and classify them
  //This is performed computing a scalar product on the score vectors for every document
  //Computation might take some time
 //temporary while getting "scalala" to work:
   val scalarProduct = new Array[Array[Double]](datasetSize,datasetSize)
   //transpose array to perform row Array operations instead of column based operations
   val tfidfTranspose = tfidfArray.transpose
    for (i <- 0 to datasetSize -1){
    println(i)
    for (j <- 0 to datasetSize -1){
      if(i!=j){
    	//Here operations take cost of length O(dictionary length)       
        scalarProduct(i)(j) = dotProduct(tfidfTranspose(i), tfidfTranspose(j))
      }else{
        //does not mean anything
        scalarProduct(i)(j) = 0
      }
    }
  }
  
  //(i,j) of scalarProduct represents the scalar product of document i and document j. Now we have
  // to sort it in order in a list to return the closest documents to a given document
  //we have weights (higher weight/score) means being closer document-to-document wise
   
   
  }
	  
   //reading text from given file
	  //we create a val string that we will read 
	  
	  //First test with 6 papers, reading them, counting occurences of all words, creating a dictionnary
val source1 = scala.io.Source.fromFile("PapersDataset/1569550425.txt")
val source2 = scala.io.Source.fromFile("PapersDataset/1569551347.txt")
val source3 = scala.io.Source.fromFile("PapersDataset/1569551535.txt")
val source4 = scala.io.Source.fromFile("PapersDataset/1569551539.txt")
val source5 = scala.io.Source.fromFile("PapersDataset/1569551541.txt")
val source6 = scala.io.Source.fromFile("PapersDataset/1569551751.txt")

//defining the number of documents:
val datasetSize: Int = 6

val text1 = source1 .mkString
source1.close ()
val text2 = source2 .mkString
source2.close ()
val text3 = source3 .mkString
source3.close ()
val text4 = source4 .mkString
source4.close ()
val text5 = source5 .mkString
source5.close ()
val text6 = source6 .mkString
source6.close ()


//Computing the number of occurences of each word type grouping by every element with its kind
//seperate on white spaces (split)
val occurences1 = text1.split("\\s+").groupBy(x=>x)
val occurences2 = text2.split("\\s+").groupBy(x=>x)
val occurences3 = text3.split("\\s+").groupBy(x=>x)
val occurences4 = text4.split("\\s+").groupBy(x=>x)
val occurences5 = text5.split("\\s+").groupBy(x=>x)
val occurences6 = text6.split("\\s+").groupBy(x=>x)


//now we want to have a map between words and the number of occurences
//create an array for easier manipulation
val counts = new Array[Map[java.lang.String,Int]](6)

counts(0) = occurences1.mapValues(x=>x.length)
counts(1) = occurences2.mapValues(x=>x.length)
counts(2) = occurences3.mapValues(x=>x.length)
counts(3) = occurences4.mapValues(x=>x.length)
counts(4) = occurences5.mapValues(x=>x.length)
counts(5) = occurences6.mapValues(x=>x.length)

val k2 = counts(2).keys


//only working with keys: 

val klist2 = k2.toList

//println(klist)

// checking if 2 documents have words in common:
/*
for (k <- counts1.keys){
  for (j <- counts2.keys){
    if (k == j) {
    println(k)
	}
	}
}
*/

// working with lists:
//only working with keys for now:

//Create an array of lists to store all different lists of keys:
val countsList = new Array[List[java.lang.String]](6)

countsList(0) = counts(0).keys.toList
countsList(1) = counts(1).keys.toList
countsList(2) = counts(2).keys.toList
countsList(3) = counts(3).keys.toList
countsList(4) = counts(4).keys.toList
countsList(5) = counts(5).keys.toList



// find the number of distinct words:
println("the number of distinct words in text 1 is: " + countsList(0).length)
println("the number of distinct words in text 2 is: " + countsList(1).length)
println("the number of distinct words in text 3 is: " + countsList(2).length)
println("the number of distinct words in text 4 is: " + countsList(3).length)
println("the number of distinct words in text 5 is: " + countsList(4).length)
println("the number of distinct words in text 6 is: " + countsList(5).length)


val textsList = List(countsList(0), countsList(1), countsList(2), countsList(3), countsList(4), countsList(5))
val texts = textsList.flatten
val textsLength = texts.length


//building dictionnary:
//find unique words in texts:

val dictionnary = texts.removeDuplicates.sort(_<_)

println("The total length of the dictionnary is given by: " + dictionnary.length)

println("the total length of the list is: " + textsLength)

//Computing TF value:

def tf(term: String, document: Int): Double = {
val freq = 0
  if (counts(document).contains(term)){
    val freq = counts(document)(term)
  }else{
    val freq = 0
  }

 //normalization with respect to the documents length to prevent any bias:
val keyValue = counts(document).values
val sum = keyValue.reduceLeft(_+_)
val normalizedFreq = freq/sum

return normalizedFreq
}

//Computing IDF value

def idf(term: String): Double = {
  //math.log(size / index.getDocCount(term))
  // take the logarithm of the quotient of the number of documents by the documents where term t appears
  var appearances = 1
  for(i <- 0 to datasetSize-1){
  var termFreq = tf(term,i)
  if (termFreq != 0){
    appearances += 1
  }
  }
  return math.log(datasetSize/appearances)
}

def tfidf(term:String, document: Int) : Double = {
  //create tfidf matrix
  //tfidf = tf*idf
  
val tfidf = tf(term,document)*idf(term)
return tfidf
 
}

//defining scala product for array vector operations
def dotProduct[T <% Double](as: Iterable[T], bs: Iterable[T]) = {
   require(as.size == bs.size)
   (for ((a, b) <- as zip bs) yield a * b) sum
 }

}


//println("the new total length of the list is: " + textCounts.length)

//this shows all the words from all the texts and their occurences, not the list of unique words (to fix)
//println(texts)



//val tokenizer = new DefaultTokenizer()

//val tokenized = tokenizer.tokenize(lines)
//println(tokenized)
  //showing content of the file
//println(lines)
	
	//val classifier = new SimpleClassifier();
	//classifier.setSearchWord( "java" );
	//val sentence : java.lang.String = "This is a sentance about java"
	//println( "The string " + sentence +	" contains the word java:" + classifier.isMatch(sentence) );




object WordReader {

//val rootDir = new File("/PapersDataset")
//if (!rootDir.exists) throw new IllegalArgumentException(rootDir + " does not exist")


///////////////////////////OTHER CODE//////////////////////////////////
/*
val rootDir = new File("/PapersDataset")
if (!rootDir.exists) throw new IllegalArgumentException(rootDir + " does not exist")

/** Iterates over all files under rootDir, opens each one and passes it to the function */
def files(rootDir: File)(process: File => Unit) {
  for (dir <- rootDir.listFiles; if dir.isDirectory) {
    println("Processing" + dir)
    for (file <- dir.listFiles; if file.isFile) {
      process(file)
    }
  }
}
 
val t1 = System.currentTimeMillis
var counts = Map.empty[String, Int].withDefaultValue(0)
files(rootDir) { file => 
  file.split("""\W+""").foreach{ word => counts = counts(word.toLowerCase) += 1 }
}

 
println("Writing counts in decreasing order")
write(counts, "counts-descreasing-scala") {_._2 > _._2}
 
println("Writing counts in alphabetical order")
write(counts, "counts-alphabetical-scala") {_._1 < _._1}
 
val t2 = System.currentTimeMillis
println("Finished in " + ((t2 - t1)/1000.0) + " seconds");
 
/** Writes the specified map to the specified file in tab-delimited format, sorted accordingly. */
def write[K, V](map: Map[K, V], file: String)(sort: (Tuple2[K, V], Tuple2[K, V]) => Boolean) {
  using (new PrintWriter(new FileWriter(file))) { out => 
    map.toList.sort(sort).foreach { pair => out.println(pair._1 + "\t" + pair._2) }
  }
}
 
/** Converts a File to a String. */
implicit def file2String(file: File): String = {
  val builder = new StringBuilder
  using (new BufferedReader(new FileReader(file))) { reader => 
    var line = reader.readLine
    while (line != null) {
      builder.append(line).append('\n')
      line = reader.readLine
    }
  }
  builder.toString
}
 */

/** Performs some operation on the specified closeable object and then ensures it gets closed. */
//def using[Closeable <: {def close(): Unit}, B](closeable: Closeable)(getB: Closeable => B): B = 
  //try {
   // getB(closeable)
  //} finally {
    //closeable.close()
  //}



//TEST reading file: 
/*// will store the words read from the file
List<String> wordList = new ArrayList<String>();

BufferedReader br = null;
try {
// attempt to open the words file
val br = new BufferedReader( new FileReader( "words.txt" ) )

val word = new java.lang.String

// loop and read a line from the file as long as we dont get null
while( ( word = br.readLine() ) != null )
// add the read word to the wordList
wordList.add( word );
} catch( IOException e ) {
e.printStackTrace();
} finally {
try {
// attempt the close the file
br.close();
} catch( IOException ex ) {
ex.printStackTrace();
}
}

// initialize a new string array equal to the size of the wordList
val words = new java.lang.String


// call the wordList's toArray method to and transfer items from
// wordList to our string array words
wordList.toArray( words );

// loop and display each word from the words array
for(i <- 0 to words.length)
println( words(i) );
}
}
 */
}
