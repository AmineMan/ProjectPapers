package paper

import java.io._
import java.util.Scanner
import net.sf.classifier4J._
import math._
import java.util.ArrayList
import java.io.{Closeable, File, FileWriter, PrintWriter}
import collection.mutable.ArrayBuffer
import scala.io.BufferedSource
import scala.collection.immutable.List
import scalala.ScalalaConsole
import scalala.tensor.dense.DenseMatrix



trait bagOfWords {


	//return list of papers linked?
	//def compare(papers : List[Paper], limit : Int) : List[Paper] = {
	//papers.map(p => {
	//if (p.meta.get("linked") == None) {

	//}
	//}
	def main(args: Array[String]): Unit = {

			val directory : java.io.File = new java.io.File("PapersDataset/")
	val papersSorted = getScores(directory)
	for(i<- 0 to 27){
		for(j<- 0 to 27){
			println(papersSorted(i)(j))
		}
	}

	}


	//}



	//compare based on scores and return List[Paper]
	def compareBoW(papers : List[Paper], limit : Int) : List[Paper] = {
 papers.map(p => {
      // Check that paper isn't already linked
      if (p.meta.get("linked") == None) {
        // Get list of papers that aren't current paper
        val otherPapers = papers.filter(p != _)

        // Compare to every other paper
        // Test
        val weights : List[Double] = for (other <- otherPapers) yield getScores(new java.io.File("PapersDataset/"))(p.index)(other.index)

        // Make links
        //val links = for ((p,w) <- otherPapers.zip(weights) if w >= limit) yield Link(p.id,w)
        val links = for ((p,w) <- otherPapers.zip(weights) if w >= 0.0) yield Link(p.index,w)

        // Add links to paper, and set it as linked
        val result = p.setLinks(links).setMeta("linked", "yes")

        // Save result
        Cache.save(result, Cache.linked)

        result
      }
      else p
    })
	}

	def getScores(directory: java.io.File): Array[Array[Double]] ={

			val filesList = new java.io.File(directory.toString).listFiles.filter(_.getName.endsWith(".txt"))

					val datasetSize = filesList.length

					//Initialisation of arrays
					//Array storing the different sources and the different texts
					val source = new Array[scala.io.BufferedSource](filesList.length)
					val text = new Array[java.lang.String](filesList.length)

					val occurences = new Array[Map[java.lang.String,Array[java.lang.String]]](filesList.length)
					//now we want to have a map between words and the number of occurences
					//create an array for easier manipulation
					val counts = new Array[Map[java.lang.String,Int]](filesList.length)
					//Create an array of lists to store all different lists of keys:
					val countsList = new Array[List[java.lang.String]](filesList.length)

					//List holding all the list of strings of all the texts
					var textsList = List[java.lang.String]()
					//reading from every entry of the list:
					for (k <- 0 to filesList.length-1){
						
						source(k) = scala.io.Source.fromFile(filesList(k))
						text(k) = source(k).mkString
						//leave out unecessary characters from the analysis
						text(k) = clean(text(k))
						source(k).close ()

						occurences(k) = text(k).split("\\s+").groupBy(x=>x)

						// create a map of the keys of the text with their occurences
						counts(k) = occurences(k).mapValues(x=>x.length)

						//only working with keys for now, creating a list of keys for every text:
						countsList(k) = counts(k).keys.toList

						if(k == 0){
							textsList = countsList(k)
						}else{
							textsList = textsList ::: countsList(k)
						}
					}


			//building dictionary:
			//find unique words in texts:

			//val texts = textsList.flatten
			val textsLength = textsList.length

					val dictionary = textsList.removeDuplicates.sort(_<_)

					// we compute the array of scores for the vectors of words for every document
					val tfidfArray = new Array[Array[Double]](dictionary.length,datasetSize)

					println("Computing tfidf array... " + dictionary.length)
					for (i <- 0 to dictionary.length -1){
						println(i)
						for (j <- 0 to datasetSize -1){
							//compute tfidf value for word i and document j
							tfidfArray(i)(j) = tfidf(dictionary(i),j,datasetSize,counts)
									//println(tfidfArray(i)(j))
						}
					}
			println("Computing tfidf array: Complete...")
			//once we have the scores we can compute the absolute distance between papers and classify them
			//This is performed computing a scalar product on the score vectors for every document
			//Computation might take some time
			//temporary while getting "scalala" to work:
			val scalarProduct = new Array[Array[Double]](datasetSize,datasetSize)
			//transpose array to perform row Array operations instead of column based operations
			val tfidfTranspose = tfidfArray.transpose
			println("Computing scalar product array")
			for (i <- 0 to datasetSize -1){
				//println(i)
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
			//return array of scores
			println("Computing scalar product array: Done...")			
			// map every score with the paper ID
			//for every paper sort according to scores
			println("Sorting accordingly...")
			val a = 0 until datasetSize
			var positions = new Array[List[(Double,Int)]](datasetSize)
			for(k <- 0 to datasetSize-1){
				positions(k) = (scalarProduct(k).zip(a)).toList.sort(_._1 < _._1 )
			}


			//return sorted scores with according paper

			return scalarProduct
					//(i,j) of scalarProduct represents the scalar product of document i and document j. Now we have
					// to sort it in order in a list to return the closest documents to a given document
					//we have weights (higher weight/score) means being closer document-to-document wise
	}


	// Code has to be made generic for any text file parsed and for the whole dataset to be accurate

	//reading text from given file
	//Loading the List of all available text files in directory


	/*
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

	 */


	//building dictionnary:
	//find unique words in texts:


	//println("The total length of the dictionnary is given by: " + dictionary.length)

	//println("the total length of the list is: " + textsLength)

	//Computing TF value:

	def tf(term: String, document: Int, counts: Array[Map[java.lang.String,Int]]): Double = {
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

	def idf(term: String, datasetSize : Int, counts: Array[Map[java.lang.String,Int]]): Double = {
			//math.log(size / index.getDocCount(term))
			// take the logarithm of the quotient of the number of documents by the documents where term t appears
			var appearances = 1
					for(i <- 0 to datasetSize-1){
						var termFreq = tf(term,i,counts)
								if (termFreq > 0){
									appearances += 1
								}
					}
			return math.log(datasetSize/appearances)
	}

	def tfidf(term:String, document: Int, datasetSize : Int, counts: Array[Map[java.lang.String,Int]]) : Double = {
			//create tfidf matrix
			//tfidf = tf*idf

			val tfidf = tf(term,document,counts)*idf(term,datasetSize,counts)
					return tfidf

	}

	//defining scala product for array vector operations
	def dotProduct[T <% Double](as: Iterable[T], bs: Iterable[T]) = {
		require(as.size == bs.size)
		(for ((a, b) <- as zip bs) yield a * b) sum
	}

	//replace all characters of a string except for a-z or A-Z and 0-9 and finally _: 
	def clean(in : String) =  if (in == null) "" else in.replaceAll("[^a-zA-Z0-9_]", " ")

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

}
