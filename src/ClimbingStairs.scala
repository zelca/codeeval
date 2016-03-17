/**
  * See <a href="https://www.codeeval.com/open_challenges/64/">Climbing Stairs</a>
  *
  * Appears to be the Fibonacci numbers.
  */
object ClimbingStairs extends App {

  val file =
    if (args.length > 0)
      args(0)
    else {
      val file = getClass.getSimpleName.init
      getClass.getResource(file).getPath
    }

  val lines = scala.io.Source.fromFile(file).getLines()

  lines.map(_.toInt).map(calc(_, 1, 1)).foreach(println)

  import scala.annotation.tailrec

  @tailrec
  def calc(count: Int, prev: BigInt, curr: BigInt): BigInt = count match {
    case 1 => curr
    case x => calc(x - 1, curr, prev + curr)
  }

}
