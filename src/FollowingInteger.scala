/**
  * See <a href="https://www.codeeval.com/open_challenges/44/">Following Integer</a>
  */
object FollowingInteger extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.collect {
    case Input(number, digits) => eval(number, digits)
  } foreach println

  import scala.annotation.tailrec

  @tailrec
  def eval(number: Int, digits: List[Char]): Int = {
    val result = compose(digits, Nil).sorted.find(_ > number)
    if (result.isEmpty) eval(number, '0' :: digits) else result.get
  }

  def compose(digits: List[Char], res: List[Char]): List[Int] = digits match {
    case Nil =>
      res.reverse.mkString.toInt :: Nil
    case _ =>
      (for (next <- digits if res != Nil || next != '0') yield compose(digits diff List(next), next :: res)).flatten
  }

  object Input {

    // 115
    def unapply(line: String) = Some(line.toInt, line.toList)

  }

}