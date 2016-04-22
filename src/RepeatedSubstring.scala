/**
  * See <a href="https://www.codeeval.com/open_challenges/53/">Repeated Substring</a>
  */
object RepeatedSubstring extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.map(line => eval(line, line.length / 2)).foreach(println)

  def eval(text: String, pos: Int): String = pos match {
    case 0 => "NONE"
    case _ =>
      text.sliding(pos).find(segment => valid(segment, text.drop(text.indexOf(segment) + segment.length))) match {
        case Some(res) => res.mkString
        case None => eval(text, pos - 1)
      }
  }

  def valid(text: String, segment: String): Boolean =
    text.trim.nonEmpty && segment.contains(text)

}