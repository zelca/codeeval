/**
  * See <a href="https://www.codeeval.com/open_challenges/188/">Distinct Triangles</a>
  */
object DistinctTriangles extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.collect {
    case Input(vertex, edges) =>
      vertex.flatMap(eval(_, edges, 0)).filter(x => x.head == x.last).map(_.toSet).toSet
  } foreach {
    case result => println(result.size)
  }

  def eval(vertex: Int, edges: Set[(Int, Int)], deep: Int): List[List[Int]] =
    deep match {
      case 3 =>
        (vertex :: Nil) :: Nil
      case _ =>
        val filtered = edges.filter(vertex == _._1).toList
        filtered.flatMap(e => eval(e._2, edges - e - e.swap, deep + 1).map(e._1 :: _))
    }

  object Input {

    // 4 5;0 2,0 1,1 2,1 3,2 3
    def unapply(line: String) =
      line.split(";").toList.map(_.split("( |,)").map(_.toInt).toList) match {
        case params :: points :: Nil =>
          val vertex = 0 until params.head
          val edges = points.grouped(2).flatMap(x => Set((x.head, x.last), (x.last, x.head)))
          Some(vertex, edges.toSet)
        case _ => None
      }

  }

}
