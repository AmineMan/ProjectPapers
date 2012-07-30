package paper

//  {"nodes":[
//  	{"name":"Myriel"},
//  	{"name":"Napoleon"},
//  	{"name":"Mlle.Baptistine"},
//  	{"name":"Mme.Magloire"}],
//  
//   "links":[
//  	{"source":1,"target":3,"value":1},
//  	{"source":2,"target":1,"value":50},
//  	{"source":3,"target":2,"value":100},
//  	{"source":0,"target":1,"value":1}]
//  }

trait Graphs {

  def getGraph(papers : List[Paper]) : Graph = {
    // Add all papers as nodes
    val nodes : List[Node] = for (p <- papers) yield makeNode(p)

    // Then create all edges
    val edges : List[Edge] = for (p <- papers; e <- makeEdges(p, nodes)) yield e

    return new Graph(nodes, edges)
  }

  def makeNode(paper : Paper) : Node = {
    println("Making node for " + paper.id)
    Node(paper.id, paper.meta("xmlpapertitle"), paper.meta("xmlauthors"), paper.meta("pdf"), paper.meta("xmldate"), paper.meta("xmlroom"))
  }
  
  def makeEdges(paper : Paper, nodes : List[Node]) : List[Edge] = {
    // make edge
    val edges = for (link <- paper.links) yield (makeEdge(paper.index, link))
    // Sort edges by weight and pick the n biggest
    val l = math.min(4, edges.length)
    return edges.sortWith(_.weight > _.weight).take(l)
  }

  def makeEdge(index : Int, link : Link) : Edge = Edge(index, link.index, link.weight)

}

class Graph(nodes : List[Node], edges : List[Edge]) {

  def save : Unit = {
    val f = new java.io.File("data.json")
    val p = new java.io.PrintWriter(f)
    p.println(toString)
    p.close
  }

  override def toString : String = {
    var ret : String = "{\n"

    // add nodes
    ret += "\"nodes\":" + nodes.mkString("[\n  ",",\n  ","\n],") + "\n"

    // add edges
    ret += "\"links\":" + edges.mkString("[\n  ",",\n  ","\n]") + "\n\n"

    // End
    ret += "}"

    return ret
  }
}

case class Node(id : Int, title : String, authors : String, pdf : String, date : String, room : String) {
  override def toString : String = {
    var ret : String = "{"
    ret += "\"id\":" + id + ",\n   "
    ret += "\"title\":\"" + Escape(title) + "\",\n   "
    ret += "\"authors\":\"" + Escape(authors) + "\",\n   "
    ret += "\"pdf\":\"" + Escape(pdf) + "\",\n   "
    ret += "\"date\":\"" + Escape(date) + "\",\n   "
    ret += "\"room\":\"" + Escape(room) + "\""
    ret += "}"
    return ret
  }
}

case class Edge(from : Int, to : Int, weight : Int) {
  override def toString : String = "{\"source\":" + from + ",\"target\":" + to + ",\"value\":" + weight + "}"
}

/** Escapes a raw string for use in HTML.*/
object Escape
{
	def apply(s: String) =
	{
		val out = new StringBuilder
		for(i <- 0 until s.length)
		{
			s.charAt(i) match
			{
				case '>' => out.append("&gt;")
				case '&' => out.append("&amp;")
				case '<' => out.append("&lt;")
				case '"' => out.append("&quot;")
				case '\n' => out.append(" ")
				case '\\' => out.append("\\\\")
				case c => out.append(c)
			}
		}
		out.toString
	}
}
