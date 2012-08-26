/**
 * Implementation of the Latent semantic indexing for text comparison using an underlying bag of words model
 * The code is similar to bagOfWords.scala except that we will be using matrices and linear algebra to be able 
 * to compute LSI and compare our documents
 * 
 * 
 * 
 */
import scalala.tensor.dense.DenseMatrix._
import scalala.operators.OpMulRowVectorBy$
import scalala._
import org.netlib._

object bagOfWordsLSI {

  
  def main(args : Array[String]): Unit = {
    val directory : java.io.File = new java.io.File("PapersDataset/")
    val mat = createTDMatrix(directory)

  }
  
	//Methods to compute the term-document matrix 
	def tf(term: String, document: Int, counts: Array[Map[java.lang.String,Int]]): Double = {
			val keyValue = counts(document).values
					val normalisationTerm = keyValue.max
					//test without normalisation	
					if (counts(document).contains(term)){
						val freq = counts(document)(term)
								val normalizedFreq = freq/normalisationTerm					
								return freq				
					}else{					 
						return 0.0
					}
	}

	//Computing IDF value
	def idf(term: String, datasetSize : Int, counts: Array[Map[java.lang.String,Int]]): Double = {
			//math.log(size / index.getDocCount(term))
			// take the logarithm of the quotient of the number of documents by the documents where term t appears
			var appearances = 0
					for(i <- 0 to datasetSize-1){
						if (counts(i).contains(term)){
							appearances += 1
						}
					}	
			return math.log(datasetSize/appearances)
	}

	def tfidf(term:String, document: Int, datasetSize : Int, counts: Array[Map[java.lang.String,Int]]) : Double = {
			//Create tfidf matrix
			//tfidf = tf*idf

			val tfidf = tf(term,document,counts)*idf(term,datasetSize,counts)
					return tfidf

	}

	//Creating the matrix:
	def createTDMatrix(directory: java.io.File): scalala.tensor.dense.DenseMatrix[Double] = {

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
								//println(counts(k))
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

			val textsLength = textsList.length

					val dictionary = textsList.removeDuplicates.sort(_<_)

					// we compute the Matrix of scores for the vectors of words for every document
					//construct it as a vector to convert it as a Matrix
					val tfidfVector = new Array[Double](dictionary.length*datasetSize)
					var j = 0 
					
					println("Computing tfidf vector... with a dictionary of length " + dictionary.length + " and wait up to " + dictionary.length*datasetSize)
			for (i <- 0 to dictionary.length*datasetSize-1){
				println(i)
				//compute tfidf value for word i and document j
				//check if we have reached the length of the dictionary we change document and compute values
				if (i % dictionary.length == 0 & i != 0){
					j += 1	
				}

				tfidfVector(i) = tfidf(dictionary(i%dictionary.length),j,datasetSize,counts)							
			}
	
	println("Computing tfidf vector: Complete...")

	val termDocMatrix = new scalala.tensor.dense.DenseMatrix[Double](dictionary.length,datasetSize,tfidfVector)
	
	//once having the termDocMatrix, compute the SVD and then compute cosine similarity on the new matrix:
	
	val (r,s,d) = scalala.library.LinearAlgebra.svd(termDocMatrix)
	
	val k = 200
	//temporary keep all singular values greater than mean:	
	val keptValues = s.findAll(_>s.mean)
	val so = s(keptValues)
	
	println("the decomposition of so gives: ... " + so)
	//compute cosine similarity:
	//val similarityMatrix = DenseMatrix.zeros[Double](datasetSize,datasetSize)

	//not optimal version:
	for (i <- 0 to datasetSize-1){
	  for (j <- 0 to datasetSize-1){
	    //Compute scalar product between two matrices
	    val firstColumn = termDocMatrix(0 to datasetSize-1,i)
	    val secondColumn =  termDocMatrix(0 to datasetSize-1,j)
	   // similarityMatrix(i,j) = firstColumn.dot(secondColumn)	 
	    //Compute 2nd norm and output cosine similarity
	    val firstColumnNorm = firstColumn.norm(2)
	    val secondColumnNorm = secondColumn.norm(2)
	    
	    //similarityMatrix(i,j) = similarityMatrix(i,j)/(firstColumnNorm*secondColumnNorm)
	    
	  }
	}
	 
	
	//test return
	//return similarityMatrix
return termDocMatrix
}

	/*
def getScores(matrixOfScores: DenseMatrix[Double], column: Int): List[Double] ={

		//val matrixOfScoresTranspose = matrixOfScores.transpose

		//	  return matrixOfScoresTranspose(column).toList

}
*/

def clean(in : String) =  if (in == null) "" else in.replaceAll("[^a-zA-Z_]", " ")

}