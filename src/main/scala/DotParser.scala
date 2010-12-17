object DotParser extends scala.util.parsing.combinator.RegexParsers {
  lazy val graph =
    "digraph" ~> "Tasks" ~> "{" ~> rep((not("}") ~> command)) <~ "}" 

  lazy val nonblank = "\\p{Graph}+".r

  lazy val command: Parser[Option[Nodes]] = 
    (procInstr ^^ { _ => None }) | (nodes ^^ { Some(_) })

  lazy val procInstr = comment | rankdir | nodeStyle | ";"
  lazy val rankdir = ("rankdir" ~ "=" ~ "LR") ^^ { _ => None }
  lazy val nodeStyle =
    ("node" ~ "[" ~ rep(not("]") ~ ".".r) ~ "]") ^^ { _ => None }
  lazy val comment = "(?m)//.+$".r ^^ { _ => None }

  lazy val nodes = subgraph | edgespec

  sealed trait Nodes 
  case class SubGraph(name: String, content: List[Nodes]) extends Nodes

  lazy val subgraph =
    (
      ("subgraph" ~> identifier <~ "{") ~ (rep(not("}") ~> command) <~ "}")
    ) map { case id ~ content => SubGraph(id, content.flatten) }
  
  lazy val identifier = "\\p{Alnum}+".r | ("\"" ~> ("[^\"]*".r) <~ "\"")
  lazy val nodespec = identifier

  case class EdgeSpec(content: List[String]) extends Nodes
  lazy val edgespec = rep1sep(nodespec, "->") ^^ { x => EdgeSpec(x) }

  def main(args: Array[String]) {
    val content = 
      parseAll(graph, new java.io.FileReader("../plan.dot")) map (_.flatten)

    content match {
      case Success(nodes, _) =>
        def flatten(x: Nodes): List[String] = 
          x match {
            case SubGraph(_, content) => content.flatMap(flatten)
            case EdgeSpec(content) => content
          }
        println(nodes.flatMap(flatten).toSet.size)
      case x => println(x)
    }
  }
}
