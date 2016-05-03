/**
  * See <a href="https://www.codeeval.com/open_challenges/47/">Palindromic Ranges</a>
  */
object PalindromicRanges extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.collect {
    case Input(range) =>
      val palindromes = range.map(x => convert(x.toString.toList)).toList
      eval(palindromes)
  } foreach println

  import scala.annotation.tailrec

  @tailrec
  def convert(x: List[Char]): Int =
    if (x.size <= 1) 1 else if (x.head == x.last) convert(x.tail.init) else 0

  def eval(i: List[Int]): Int = i match {
    case Nil => 0
    case x :: xs =>
      i.foldLeft((0, 0)) {
        (a, x) => ((a._1 + x) % 2, a._2 + 1 - (a._1 + x) % 2)
      }._2 + eval(xs)
  }

  object Input {

    // 1 2
    def unapply(line: String) = line.split(" ").toList match {
      case left :: right :: Nil => Some(left.toInt to right.toInt)
      case _ => None
    }

  }

}