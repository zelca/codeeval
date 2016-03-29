/**
  * See <a href="https://www.codeeval.com/open_challenges/151/">Cracking eggs</a>
  */
object CrackingEggs extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.collect {
    case Input(tries, floors) =>
      (1 to floors).find(i => floor(tries, i) >= floors).get
  } foreach println

  def floor(e: Int, t: Int): Int = (e, t) match {
    case (1, n) => n
    case (x, 0) => 0
    case (x, n) => 1 + floor(x - 1, n - 1) + floor(x, n - 1)
  }

  object Input {

    // 2 100
    def unapply(line: String) = line.split(" ").toList match {
      case tries :: floors :: Nil => Some(tries.toInt, floors.toInt)
      case _ => None
    }

  }

}
