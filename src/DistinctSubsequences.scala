/**
  * See <a href="https://www.codeeval.com/open_challenges/69/">Distinct Subsequences</a>
  */
object DistinctSubsequences extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.collect {
    case Input(seq, subseq) => (eval(seq, subseq, Nil), subseq)
  } foreach {
    case (result, subseq) => println(result.count(_ == subseq))
  }

  import scala.annotation.tailrec

  @tailrec
  def eval(seq: List[Char], subseq: String, res: List[String]): List[String] =
    seq match {
      case Nil => res
      case x :: xs =>
        val filtered = (x.toString :: res.map(_ + x)).filter(subseq.contains)
        eval(xs, subseq, filtered ::: res)
    }

  object Input {

    // babgbag,bag
    def unapply(line: String) = line.split(",").toList match {
      case seq :: subseq :: Nil => Some(seq.toList, subseq)
      case _ => None
    }

  }

}