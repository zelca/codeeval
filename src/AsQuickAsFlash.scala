/**
  * See <a href="https://www.codeeval.com/open_challenges/239/">As quick as a flash</a>
  *
  * It's better to use a mutable array for performance. Anyway it's still the solution.
  */
object AsQuickAsFlash extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines()

  lines.collect {
    case Input(numbers) => sort(numbers)
  } foreach println

  def sort(numbers: List[Int]): Int = numbers match {
    case Nil => 0
    case x :: Nil => 0
    case x :: xs =>
      val parts = rpart(x, xs.reverse, Nil, Nil)
      sort(parts._1) + 1 + sort(parts._2)
  }

  import scala.annotation.tailrec

  @tailrec
  def lpart(p: Int, n: List[Int], l: List[Int], r: List[Int]): (List[Int], List[Int]) =
    n match {
      case Nil => (l.reverse, r)
      case x :: xs if x >= p => rpart(p, xs.reverse, l, x :: r)
      case x :: xs => lpart(p, xs, x :: l, r)
    }

  @tailrec
  def rpart(p: Int, n: List[Int], l: List[Int], r: List[Int]): (List[Int], List[Int]) =
    n match {
      case Nil => (l.reverse, r)
      case x :: xs if x <= p => lpart(p, xs.reverse, x :: l, r)
      case x :: xs => rpart(p, xs, l, x :: r)
    }

  object Input {

    // 5 2 6 1 3 4
    def unapply(line: String) = Some(line.split(" ").map(_.toInt).toList)

  }

}