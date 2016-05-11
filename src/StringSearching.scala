/**
  * See <a href="https://www.codeeval.com/open_challenges/28/">String Searching</a>
  */
object StringSearching extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.collect {
    case Input(string1, string2) => eval(string1, string2, asterisk = true)
  } foreach println

  def eval(in: List[Char], res: List[Char], asterisk: Boolean): Boolean = (res, in) match {
    case (Nil, _) => true
    case ('*' :: xs, _) => eval(in, xs, asterisk = true)
    case (x :: xs, Nil) => false
    case (x :: xs, y :: ys) =>
      (x == y && eval(ys, xs, asterisk = false)) || (asterisk && eval(ys, res, asterisk))
  }

  object Input {

    // Hello,ell
    def unapply(line: String) = line.split(",").toList match {
      case string1 :: string2 :: Nil => Some(string1.replace("*", "_").toList, string2.replace("\\*", "_").toList)
      case _ => None
    }

  }

}