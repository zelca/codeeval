/**
  * See <a href="https://www.codeeval.com/open_challenges/120/">Skyscrapers</a>
  */
object Skyscrapers extends Challenge {

  type Value = ((Int, Int), Int)

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.collect {
    case Input(data) =>
      eval(sort(data), List(), Map()).toList.sorted
  } foreach {
    result => println(result.map(x => x._1 + " " + x._2).mkString(" "))
  }

  def sort(data: List[Value]): (List[Value], List[Value]) =
    (data.sortWith((v1, v2) => v1._1._1 < v2._1._1), data.sortWith((v1, v2) => v1._1._2 < v2._1._2))

  import scala.annotation.tailrec

  @tailrec
  def eval(data: (List[Value], List[Value]), stack: List[Value], res: Map[Int, Int]): Map[Int, Int] = data match {
    case (_, Nil) => res
    case (s :: ss, e :: es) if s._1._1 <= e._1._2 =>
      val updated = push(s, stack)
      if (stack.isEmpty || updated.head._2 > stack.head._2)
        eval((ss, e :: es), updated, res + (s._1._1 -> updated.head._2))
      else
        eval((ss, e :: es), updated, res)
    case (ss, e :: es) =>
      val updated = pop(e, stack)
      if (updated.isEmpty)
        eval((ss, es), updated, res + (e._1._2 -> 0))
      else if (updated.head._2 < stack.head._2)
        eval((ss, es), updated, res + (e._1._2 -> updated.head._2))
      else
        eval((ss, es), updated, res)
  }

  def push(item: Value, list: List[Value]): List[Value] = list match {
    case Nil => item :: Nil
    case x :: xs if x._2 < item._2 => item :: x :: xs
    case x :: xs => x :: push(item, xs)
  }

  def pop(item: Value, list: List[Value]): List[Value] = list match {
    case Nil => throw new IllegalStateException()
    case x :: xs if x == item => xs
    case x :: xs => x :: pop(item, xs)
  }

  object Input {

    def parse(item: String): Value = {
      val value = item.tail.init.split(",").map(_.toInt)
      ((value(0), value(2)), value(1))
    }

    // (2,3,10);(9,2,10)
    def unapply(line: String) =
      Some(line.replace(" ", "").split(";").map(parse).toList)

  }

}