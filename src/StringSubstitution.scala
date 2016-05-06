/**
  * See <a href="https://www.codeeval.com/open_challenges/50/">String Substitution</a>
  */
object StringSubstitution extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.collect {
    case Input(string, parts) => eval(parts, string)
  } foreach println

  import scala.annotation.tailrec

  @tailrec
  def eval(parts: List[String], res: String): String = parts match {
    case Nil => res.replace('a', '0').replace('b', '1')
    case x1 :: x2 :: xs => eval(xs, res.replace(x1, x2.replace('0', 'a').replace('1', 'b')))
    case _ => throw new IllegalArgumentException(parts.toString)
  }

  object Input {

    // 10011011001;0110,1001,1001,0,10,11
    def unapply(line: String) = line.split(";").toList match {
      case string :: parts :: Nil => Some(string, parts.split(",").toList)
      case _ => None
    }

  }

}
