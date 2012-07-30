package paper

abstract class PaperSource {
  def getInfo(p : Paper) : String
  def getLabel : String
}

object TalkDates extends PaperSource {

  import scala.util.Random
  import java.util.Date
  import java.sql.Timestamp
  import java.util.Calendar

  // TODO: This is just a temporary implementation
  def getInfo(p : Paper) : String = {

    // Get Calendar and Random
    var c = Calendar.getInstance
    var r = new Random

    // Set starting point as tomorrow at 8
    c.add(Calendar.DAY_OF_MONTH, 1)
    c.set(Calendar.HOUR_OF_DAY,8)
    c.set(Calendar.MINUTE,0)
    c.set(Calendar.SECOND,0)
    c.set(Calendar.MILLISECOND,0)

    // Now add between zero and 6 hours
    c.add(Calendar.HOUR, (r.nextDouble * 7).toInt)
    // Add between 0 and 5 days
    c.add(Calendar.DAY_OF_MONTH, (r.nextDouble * 6).toInt)

    // Get a timeStamp
    var t = new Timestamp(c.getTime.getTime).getTime.toString

    //var n = new Timestamp(new Date().getTime).getTime
    //var r = (n + (new Random().nextDouble * (60*60*24*4*1000)).toLong).toString
    
    return t
  }

  def getLabel : String = "date"
}

object PdfLink extends PaperSource {
  import scala.util.Random

  // TODO: This is also just a temporary thing
  def getInfo(p : Paper) : String = {
    
    var f : String = p.meta("file")
    var pdf : String = f.takeWhile(_!='.').concat(".pdf")
    return pdf;
  }

  def getLabel : String = "pdf"
}


object TalkRooms extends PaperSource {
  import scala.util.Random

  // TODO: This is also just a temporary thing
  def getInfo(p : Paper) : String = {
    
    // Return a random room between 1 and 10 
    return (new Random().nextDouble * 10).toInt.toString
  }

  def getLabel : String = "room"
}


trait ExtendPaper {

  def extend(papers : List[Paper], sources : List[PaperSource]) : List[Paper] = {

    papers.map(p => {

      var result : Paper = p

      // For each source, check if it's already added, and if not, add it
      for (s <- sources if p.hasMeta(s.getLabel)) {
        result = result.setMeta(s.getLabel, s.getInfo(p))
      }

      // Save result
      Cache.save(result, Cache.extended)

      // return result
      result
    })
  }

}
