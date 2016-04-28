/**
  * See <a href="https://www.codeeval.com/open_challenges/188/">Ugly Numbers</a>
  */
object UglyNumbers extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.collect {
    case Input(digits, coef) =>
      val numbers = compose(digits.length - 1, List()).map(x => eval(digits.tail zip x, digits.head :: Nil))
      numbers.count(ugly) * coef
  } foreach println

  def compose(count: Int, res: List[Int]): List[List[Int]] = count match {
    case 0 => res :: Nil
    case n => (-1 to 1).toList.flatMap(i => compose(count - 1, i :: res))
  }

  def eval(input: List[(Char, Int)], res: List[Char]): Long = input match {
    case Nil => res.reverse.mkString.toLong
    case (number, -1) :: xs => res.reverse.mkString.toLong - eval(xs, number :: Nil)
    case (number, 0) :: xs => eval(xs, number :: res)
    case (number, 1) :: xs => res.reverse.mkString.toLong + eval(xs, number :: Nil)
    case _ => throw new IllegalStateException()
  }

  def ugly(number: Long) = number match {
    case 0 => true
    case x if x % 2 == 0 => true
    case x if x % 3 == 0 => true
    case x if x % 5 == 0 => true
    case x if x % 7 == 0 => true
    case _ => false
  }

  object Input {

    def filter(line: List[Char]): List[Char] = line match {
      case '0' :: Nil => '0' :: Nil
      case '0' :: xs => filter(xs)
      case xs => xs
    }

    // 0000000000277
    def unapply(line: String) = {
      val filtered = filter(line.toList)
      Some(filtered, scala.math.pow(3, line.length - filtered.size).toInt)
    }

  }

}
