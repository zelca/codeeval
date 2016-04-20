/**
  * See <a href="https://www.codeeval.com/open_challenges/110/">Text to Number</a>
  */
object TextToNumber extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  val map = Map("negative" -> -1,
    "zero" -> 0, "one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4, "five" -> 5, "six" -> 6, "seven" -> 7, "eight" -> 8, "nine" -> 9,
    "ten" -> 10, "eleven" -> 11, "twelve" -> 12, "thirteen" -> 13, "fourteen" -> 14, "fifteen" -> 15, "sixteen" -> 16, "seventeen" -> 17, "eighteen" -> 18, "nineteen" -> 19,
    "twenty" -> 20, "thirty" -> 30, "forty" -> 40, "fifty" -> 50, "sixty" -> 60, "seventy" -> 70, "eighty" -> 80, "ninety" -> 90,
    "hundred" -> 100, "thousand" -> 1000, "million" -> 1000000)

  lines.map {
    _.split(" ").toList match {
      case "negative" :: xs => map("negative") * eval(xs.reverse, 1)
      case x => eval(x.reverse, 1)
    }
  } foreach println

  def eval(l: List[String], m: Int): Int = l match {
    case Nil => 0
    case x :: xs =>
      map(x) match {
        case 100 => eval(xs.tail, m) + map(xs.head) * 100 * m
        case 1000 => eval(xs, 1000)
        case 1000000 => eval(xs, 1000000)
        case i => eval(xs, m) + i * m
      }
  }

}
