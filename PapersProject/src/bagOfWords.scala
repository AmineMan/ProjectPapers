package paper
import java.io._
import java.util.Scanner
import net.sf.classifier4J._

import java.util.List;
import java.util.ArrayList;
import java.io.{Closeable, File, FileWriter, PrintWriter}


trait bagOfWords {
	
	val classifier = new SimpleClassifier();
	classifier.setSearchWord( "java" );
	val sentence : java.lang.String = "This is a sentance about java"
	println( "The string " + sentence +	" contains the word java:" + classifier.isMatch(sentence) );

}



object WordReader {
 def main(args : Array[String]): Unit = {

   //reading text from given file
val source = scala.io.Source.fromFile("PapersDataset/1569550425.txt")
val lines = source .mkString
source.close ()
val tokenizer = new DefaultTokenizer()

val tokenized = tokenizer.tokenize(lines)
println(tokenized)
//val rootDir = new File("/PapersDataset")
//if (!rootDir.exists) throw new IllegalArgumentException(rootDir + " does not exist")

  //showing content of the file
println(lines)
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
}