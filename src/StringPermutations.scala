/**
  * See <a href="https://www.codeeval.com/open_challenges/14/">String Permutations</a>
  */
object StringPermutations extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.map(_.toList.sorted).map(compose(_, Nil)).map(_.mkString(",")).foreach(println)

  def compose(letters: List[Char], res: List[Char]): List[String] = letters match {
    case Nil => res.reverse.mkString :: Nil
    case _ => letters.flatMap(char => compose(letters diff List(char), char :: res))
  }

}