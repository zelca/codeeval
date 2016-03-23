/**
  * See <a href="https://www.codeeval.com/open_challenges/6/">Longest common sequence</a>
  */
object CommonSequence extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines()

  lines.collect {
    case Input(left, right) => calc(left, right, Nil)
  } foreach {
    result => println(result.reverse.mkString)
  }

  def calc(in1: List[Char], in2: List[Char], res: List[Char]): List[Char] = in1 match {
    case Nil => res
    case x :: xs =>
      val i = in2.indexOf(x)
      val s1 = if (i >= 0) calc(xs, in2.drop(i + 1), x :: res) else Nil
      val s2 = calc(xs, in2, res)
      if (s1.size > s2.size) s1 else s2
  }

  object Input {

    // XMJYAUZ;MZJAWXU
    def unapply(line: String) = line.split(";").toList match {
      case left :: right :: Nil => Some(left.toList, right.toList)
      case _ => None
    }

  }

}