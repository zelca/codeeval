/**
  * See <a href="https://www.codeeval.com/open_challenges/77/">Da Vyncy</a>
  */
object DaVyncy extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.map(_.split(";").toList).map(concat(_)).foreach(println)

  def concat(parts: List[String]): String = {
    val filtered = parts.filterNot(y => parts.exists(x => x != y && x.contains(y)))
    val distances = for (x <- filtered; y <- filtered if x != y) yield (x, y, dist(x, y))
    distances match {
      case Nil => parts.mkString
      case _ =>
        val max = distances.maxBy(x => x._3)
        concat(max._1.dropRight(max._3) + max._2 :: (filtered diff List(max._1, max._2)))
    }
  }

  lazy val dist: (String, String) => Int = {
    case (s1, s2) if s2.startsWith(s1) => s1.length
    case (s1, s2) => dist(s1.tail, s2)
  }

}
