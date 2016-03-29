/**
  * See <a href="https://www.codeeval.com/open_challenges/239/">As quick as a flash</a>
  *
  * It's better to use a mutable array for performance. Anyway it's still the solution.
  */
object AsQuickAsFlash extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.collect {
    case Input(numbers) => sort(numbers)
  } foreach println

  def sort(numbers: List[Int]): Int = numbers match {
    case Nil => 0
    case x :: Nil => 0
    case x :: xs =>
      val parts = splitRight(x, xs.reverse, Nil, Nil)
      sort(parts._1) + 1 + sort(parts._2)
  }

  import scala.annotation.tailrec

  @tailrec
  def splitLeft(pivot: Int, list: List[Int], left: List[Int], right: List[Int]): (List[Int], List[Int]) =
    list match {
      case Nil => (left.reverse, right)
      case x :: xs if x >= pivot => splitRight(pivot, xs.reverse, left, x :: right)
      case x :: xs => splitLeft(pivot, xs, x :: left, right)
    }

  @tailrec
  def splitRight(pivot: Int, list: List[Int], left: List[Int], right: List[Int]): (List[Int], List[Int]) =
    list match {
      case Nil => (left.reverse, right)
      case x :: xs if x <= pivot => splitLeft(pivot, xs.reverse, x :: left, right)
      case x :: xs => splitRight(pivot, xs, left, x :: right)
    }

  object Input {

    // 5 2 6 1 3 4
    def unapply(line: String) = Some(line.split(" ").map(_.toInt).toList)

  }

}