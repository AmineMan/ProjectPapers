package paper

import java.io._
import java.util.Scanner
import net.sf.classifier4J._


import java.util.ArrayList;
import java.io.{Closeable, File, FileWriter, PrintWriter}

import scala.List

object bagOfWords {
	def main(args : Array[String]): Unit = {
// Code has to be made generic for any text file parsed and for the whole dataset to be accurate
	  
	  
	  
   //reading text from given file
	  //we create a val string that we will read 
	  
	  //First test with 6 papers, reading them, counting occurences of all words, creating a dictionnary
val source1 = scala.io.Source.fromFile("PapersDataset/1569550425.txt")
val source2 = scala.io.Source.fromFile("PapersDataset/1569551347.txt")
val source3 = scala.io.Source.fromFile("PapersDataset/1569551535.txt")
val source4 = scala.io.Source.fromFile("PapersDataset/1569551539.txt")
val source5 = scala.io.Source.fromFile("PapersDataset/1569551541.txt")
val source6 = scala.io.Source.fromFile("PapersDataset/1569551751.txt")


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
val counts1 = occurences1.mapValues(x=>x.length)
val counts2 = occurences2.mapValues(x=>x.length)
val counts3 = occurences3.mapValues(x=>x.length)
val counts4 = occurences4.mapValues(x=>x.length)
val counts5 = occurences5.mapValues(x=>x.length)
val counts6 = occurences6.mapValues(x=>x.length)

val k = counts1.keys
val k2 = counts2.keys

//only working with keys: 
val klist = k.toList
val klist2 = k2.toList

// checking if 2 documents have words in common:
for (k <- counts1.keys){
  for (j <- counts2.keys){
    if (k == j) {
    println(k)
	}
	}
}


// working with lists:

val countsList1 = counts1.toList
val countsList2 = counts2.toList
val countsList3 = counts3.toList
val countsList4 = counts4.toList
val countsList5 = counts5.toList
val countsList6 = counts6.toList


// find the number of distinct words:
println("the number of distinct words in text 1 is: " + countsList1.length)
println("the number of distinct words in text 2 is: " + countsList2.length)
println("the number of distinct words in text 3 is: " + countsList3.length)
println("the number of distinct words in text 4 is: " + countsList4.length)
println("the number of distinct words in text 5 is: " + countsList5.length)
println("the number of distinct words in text 6 is: " + countsList6.length)


val textsList = List(countsList1, countsList2, countsList3, countsList4, countsList5, countsList6)
val texts = textsList.flatten
val textsLength = texts.length
println("the total length of the list is: " + textsLength)




//println("the new total length of the list is: " + textCounts.length)

//this shows all the words from all the texts and their occurences, not the list of unique words (to fix)
//println(texts)



//val tokenizer = new DefaultTokenizer()

//val tokenized = tokenizer.tokenize(lines)
//println(tokenized)
  //showing content of the file
//println(lines)
	}
	//val classifier = new SimpleClassifier();
	//classifier.setSearchWord( "java" );
	//val sentence : java.lang.String = "This is a sentance about java"
	//println( "The string " + sentence +	" contains the word java:" + classifier.isMatch(sentence) );

}



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
