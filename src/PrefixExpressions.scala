/**
  * See <a href="https://www.codeeval.com/open_challenges/7/">Prefix Expressions</a>
  */
object PrefixExpressions extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.map(_.split(" ").toList).map {
    case expressions =>
      eval(expressions.head, expressions.last.toInt, expressions.tail.init).toInt
  } foreach println

  def eval(exp: String, num: Int, rest: List[String]): Double = rest match {
    case Nil => num
    case _ =>
      val res = if (rest.size == 1) Nil else rest.tail.init
      exp match {
        case "+" => eval(rest.head, rest.last.toInt, res) + num
        case "*" => if (num == 0) 0d else eval(rest.head, rest.last.toInt, res) * num
        case "/" => eval(rest.head, rest.last.toInt, res) / num
      }
  }

}