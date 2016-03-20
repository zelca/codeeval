/**
  * See <a href="https://www.codeeval.com/open_challenges/64/">Climbing stairs</a>
  *
  * The Fibonacci numbers.
  */
object ClimbingStairs extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines()

  lines.map(_.toInt).map(calc(_, 1, 1)).foreach(println)

  import scala.annotation.tailrec

  @tailrec
  def calc(count: Int, prev: BigInt, curr: BigInt): BigInt = count match {
    case 1 => curr
    case x => calc(x - 1, curr, prev + curr)
  }

}