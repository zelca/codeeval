/**
  * See <a href="https://www.codeeval.com/open_challenges/144/">Digit Statistics</a>
  */
object DigitStatistics extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.collect {
    case Input(a, n) => eval(a, n)
  } foreach {
    result => println((0 to 9).map(x => x + ": " + result.getOrElse(x, 0)).mkString(", "))
  }

  def eval(a: Int, n: Long): Map[Int, Long] = {
    val cycle = digits(a, a, n).groupBy(x => x).mapValues(_.size)
    val period = n / cycle.size
    val rest = digits(a, a, n % cycle.size).groupBy(x => x).mapValues(_.size)
    cycle.map {
      case (digit, count) => digit -> (count * period + rest.getOrElse(digit, 0))
    }
  }

  def digits(x: Int, a: Int, n: Long): List[Int] = n match {
    case 0 => Nil
    case _ =>
      val next = x * a % 10
      x :: (if (next == a) Nil else digits(next, a, n - 1))
  }

  object Input {

    // 2 10
    def unapply(line: String) = line.split(" ").toList match {
      case a :: n :: Nil => Some(a.toInt, n.toLong)
      case _ => None
    }

  }

}