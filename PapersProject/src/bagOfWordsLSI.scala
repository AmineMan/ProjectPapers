package paper
/**

 * Implementation of the Latent semantic indexing for text comparison using an underlying bag of words model
 * The code is similar to bagOfWords.scala except that we will be using matrices and linear algebra to be able 
 * to compute LSI and compare our documents
 * 
 * 
 * 
 */



import scala.collection.Iterable$
import breeze.linalg.DenseVector
import scalala._
import scalala.tensor.::
import breeze.classify
import org.netlib.lapack.LAPACK
import org.netlib.util.intW
import breeze.linalg.support.{CanCopy}
import Matrix._

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
	def createTDMatrix(directory: java.io.File): breeze.linalg.DenseMatrix[Double] = {

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

			val termDocMatrix = new breeze.linalg.DenseMatrix[Double](dictionary.length,datasetSize,tfidfVector)
			//println(termDocMatrix.toString)
			println("done")
			//once having the termDocMatrix, compute the SVD and then compute cosine similarity on the new matrix:

			//val (r,s,d) = scalala.library.LinearAlgebra.svd(termDocMatrix)
			val (w,s,d) = svd(termDocMatrix)
			//val (e,f,g) = breeze.linalg.svd(termDocMatrix)
			//printing s to see the values
 //println(s.deep.mkString("\n"))
  			
//keeping k greatest singular values:
			//keep k values, test with half the length of the vector
			val emptyList = List[Int]()
			val indices = findKMax(s, scala.math.round((s.length/2)), emptyList)
			//select values of the indices in the given array s:
			val emptyArray = new Array[Double](0)
			val keptValues = selectElementsOfArray(s,indices,emptyArray)

			println(keptValues.deep.mkString("\n"))
  			
			//matrix multiplication: 

			//val so = s(keptValues)
			// Recompose the matrix...
			//println("the decomposition of so gives: ... " + so)
			//compute cosine similarity:
		//	val similarityMatrix = breeze.linalg.DenseMatrix.zeros[Double](datasetSize,datasetSize)

			//not optimal version:
		//	for (i <- 0 to datasetSize-1){
			//	for (j <- 0 to datasetSize-1){
					//Compute scalar product between two matrices
					//val firstColumn = termDocMatrix(0 to datasetSize-1,i)
							//val secondColumn =  termDocMatrix(0 to datasetSize-1,j)
							//similarityMatrix(i,j) = firstColumn.dot(secondColumn)	 
							//Compute 2nd norm and output cosine similarity
						//	val firstColumnNorm = firstColumn.norm(2)
						//	val secondColumnNorm = secondColumn.norm(2)

						//	similarityMatrix(i,j) = similarityMatrix(i,j)/(firstColumnNorm*secondColumnNorm)

			
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
	//remove an element on a given index from a given list:
	def dropIndex[T](xs: List[T], n: Int) = {
		val (l1, l2) = xs splitAt n
				l1 ::: (l2 drop 1)
	}
	
	def selectElementsOfArray(inputArray: Array[Double], inputIndices: List[Int], returnArray: Array[Double]): Array[Double] = {
		//var returnArray = new Array[Double](inputIndices.length)
		if(inputIndices == Nil){
			inputArray
		}else if(inputIndices.length != 1){		
			//Add element of the input array corresponding to the first index in the index list:
			var newreturnArray = returnArray :+ inputArray(inputIndices(0))		
			selectElementsOfArray(inputArray,dropIndex(inputIndices,0),newreturnArray)
		}else{
			var newreturnArray = returnArray :+ inputArray(inputIndices(0))	
								return newreturnArray					
		}		
	}
	
	def clean(in : String) ={  if (in == null) "" else in.replaceAll("[^a-zA-Z_]", " ")}

	//computing find method to return the k largest elements of a vector
	def findKMax(inputVector: Array[Double], k: Int, listOfIndex: List[Int]): List[Int]= {
		//temporary solution
		while(listOfIndex.length != k){
			val maxofArray = inputVector.max
					if (maxofArray != 0){
						listOfIndex:::List(maxofArray)						
						//set maximal value to 0 so it does not get taken into account again
						inputVector(inputVector.findIndexOf(x => x == maxofArray)) = 0
						findKMax(inputVector,k, listOfIndex)
					}else{
						listOfIndex
					}
		}
		return listOfIndex
	}

  def topNs(xs: Array[Double], n: Int) = {
    var ss = List[Double]()
    var min = Double.MaxValue
    var len = 0
    xs foreach { e =>
      if (len < n || e > min) {
        ss = (e :: ss).sorted
        min = ss.head
        len += 1
      }
      if (len > n) {
        ss = ss.tail
        min = ss.head
        len -= 1
      }                    
    }
    ss
  } 	
	
		
			// function converting a DenseVector to a List of Double
			def convertToList(inputVector : DenseVector[Double]) :  List[Double] = {
		val outputVector = List[Double]()
				for(i <- inputVector){
					outputVector:::List(i)
				}
		return outputVector
	}

	//redifining svd:

	//@inline private def requireNonEmptyMatrix[V](mat: Matrix[V]) =
	//if (mat.cols == 0 || mat.rows == 0)
	//throw new MatrixEmptyException

	def svd(mat: breeze.linalg.DenseMatrix[Double]):(Matrix,Array[Double],Matrix) = {
		// we do not use the matrix requirements
		//	requireNonEmptyMatrix(mat)

		val m = mat.rows
		val n = mat.cols
				//val S = DenseVector.zeros[Double](m min n)
				//matrix of zeros
		//S = denseVector(UCOL = min(m,n))
		//val S = Matrix(m min n,1){ (i:Int,j:Int) => 0 }
		val S = new Array[Double](m min n)
		val U = Matrix(m,m){ (i:Int,j:Int) => 0 }
		//val U = DenseMatrix.zeros[Double](m,m)
		val Vt = Matrix(n,n){ (i:Int,j:Int) => 0 }
		val iwork = new Array[Int](8 * (m min n) )
				val workSize = ( 3
						* scala.math.min(m, n)
						* scala.math.min(m, n)
						+ scala.math.max(scala.math.max(m, n), 4 * scala.math.min(m, n)
								* scala.math.min(m, n) + 4 * scala.math.min(m, n))
						)
						val work = new Array[Double](workSize)
						val info = new intW(0)
		//S.elements.flatten.toArray
		//U.take(m*m).flatten.toArray
		val cm = copy(mat)
		LAPACK.getInstance.dgesdd(
				"A", m, n,
				cm.data, scala.math.max(1,m),
				S, U.flatten.toArray , scala.math.max(1,m),
				Vt.flatten.toArray, scala.math.max(1,n),
				work,work.length,iwork, info)

				if (info.`val` > 0)
					throw new NotConvergedException(NotConvergedException.Iterations)
				else if (info.`val` < 0)
					throw new IllegalArgumentException()

		(U,S,Vt)
	}
	def copy[T](t: T)(implicit canCopy: CanCopy[T]): T = canCopy(t)

			class MatrixEmptyException extends IllegalArgumentException("Matrix is empty")

	class NotConvergedException(val reason: NotConvergedException.Reason, msg: String = "")
	extends RuntimeException(msg)

	object NotConvergedException {
		trait Reason
		object Iterations extends Reason
		object Divergence extends Reason
		object Breakdown extends Reason
	}

	}

//redefining matrices:
//http://www.scalaclass.com/book/export/html/1

object Matrix{
type Row = List[Double]
type Matrix = List[Row]

	def apply( rowCount:Int, colCount:Int )( f:(Int,Int) => Double ) = (
		for(i <- 1 to rowCount) yield 
			( for( j <- 1 to colCount) yield f(i,j) ).toList
			 ).toList
							
	//defining matrix transpose:
	private def transpose(m:Matrix):Matrix ={ 
		if(m.head.isEmpty) Nil else m.map(_.head) :: transpose(m.map(_.tail))
	} 
	
	//new definition of the dot product
	def dotProd(v1:List[Double],v2:List[Double]) = {
		v1.zip( v2 ).map{ t:(Double,Double) => t._1 * t._2 }.reduceLeft(_ + _)
	}
	
	def convertToArray(mat: Matrix): Array[Double]={
	  val a = mat.flatten.toArray
	  return a
	}
	
	//Added matrix multiplication
	def mXm( m1:Matrix, m2:Matrix ) = {
		for( m1row <- m1 ) yield{
			for( m2col <- transpose(m2) ) yield{
				dotProd( m1row, m2col )
			}
		}    
	}
}