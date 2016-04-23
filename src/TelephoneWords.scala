/**
  * See <a href="https://www.codeeval.com/open_challenges/59/">Telephone Words</a>
  */
object TelephoneWords extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  val letters = Map('0' -> "0", '1' -> "1", '2' -> "abc", '3' -> "def", '4' -> "ghi",
    '5' -> "jkl", '6' -> "mno", '7' -> "pqrs", '8' -> "tuv", '9' -> "wxyz")

  lines.map(l => eval(l.toList, Nil)).map(_.mkString(",")).foreach(println)

  def eval(num: List[Char], res: List[Char]): Seq[String] = num match {
    case Nil => res.mkString.reverse :: Nil
    case x :: xs => letters(x).flatMap(l => eval(num diff List(x), l :: res))
  }

}
