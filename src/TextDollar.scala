/**
  * See <a href="https://www.codeeval.com/open_challenges/52/">Text Dollar</a>
  */
object TextDollar extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  val map = Map(
    "one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4,
    "five" -> 5, "six" -> 6, "seven" -> 7, "eight" -> 8, "nine" -> 9,
    "ten" -> 10, "eleven" -> 11, "twelve" -> 12, "thirteen" -> 13, "fourteen" -> 14,
    "fifteen" -> 15, "sixteen" -> 16, "seventeen" -> 17, "eighteen" -> 18, "nineteen" -> 19,
    "twenty" -> 20, "thirty" -> 30, "forty" -> 40, "fifty" -> 50, "sixty" -> 60,
    "seventy" -> 70, "eighty" -> 80, "ninety" -> 90,
    "hundred" -> 100, "thousand" -> 1000, "million" -> 1000000).map(x => (x._2, x._1.capitalize))

  lines.map(line => eval(line.toInt) + "Dollars").foreach(println)

  def eval(in: Int): String = in match {
    case 0 => ""
    case x if in / 1000000 > 0 => eval(x / 1000000) + map(1000000) + eval(x % 1000000)
    case x if in / 1000 > 0 => eval(x / 1000) + map(1000) + eval(x % 1000)
    case x if in / 100 > 0 => eval(x / 100) + map(100) + eval(x % 100)
    case x if in < 20 => map(x)
    case x if in / 10 > 0 => map(x / 10 * 10) + eval(x % 10)
    case x => map(x)
  }

}
