/**
  * See <a href="https://www.codeeval.com/open_challenges/64/">Climbing stairs</a>
  *
  * The Fibonacci numbers.
  */
object ClimbingStairs extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.map(_.toInt).map(eval(_, 1, 1)).foreach(println)

  import scala.annotation.tailrec

  @tailrec
  def eval(count: Int, prev: BigInt, curr: BigInt): BigInt = count match {
    case 1 => curr
    case x => eval(x - 1, curr, prev + curr)
  }

}