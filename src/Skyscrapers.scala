/**
  * See <a href="https://www.codeeval.com/open_challenges/120/">Skyscrapers</a>
  */
object Skyscrapers extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  case class Building(start: Int, end: Int, height: Int)

  lines.collect {
    case Input(data) =>
      eval(sort(data), List(), Map()).toList.sorted
  } foreach {
    result => println(result.map(x => x._1 + " " + x._2).mkString(" "))
  }

  def sort(data: List[Building]): (List[Building], List[Building]) =
    (data.sortWith((v1, v2) => v1.start < v2.start), data.sortWith((v1, v2) => v1.end < v2.end))

  import scala.annotation.tailrec

  @tailrec
  def eval(data: (List[Building], List[Building]), stack: List[Building], res: Map[Int, Int]): Map[Int, Int] = data match {
    case (_, Nil) => res
    case (s :: ss, e :: es) if s.start <= e.end =>
      val updated = push(s, stack)
      if (stack.isEmpty || updated.head.height > stack.head.height)
        eval((ss, e :: es), updated, res + (s.start -> updated.head.height))
      else
        eval((ss, e :: es), updated, res)
    case (ss, e :: es) =>
      val updated = pop(e, stack)
      if (updated.isEmpty)
        eval((ss, es), updated, res + (e.end -> 0))
      else if (updated.head.height < stack.head.height)
        eval((ss, es), updated, res + (e.end -> updated.head.height))
      else
        eval((ss, es), updated, res)
  }

  def push(item: Building, list: List[Building]): List[Building] = list match {
    case Nil => item :: Nil
    case x :: xs if x.height < item.height => item :: x :: xs
    case x :: xs => x :: push(item, xs)
  }

  def pop(item: Building, list: List[Building]): List[Building] = list match {
    case Nil => throw new IllegalStateException()
    case x :: xs if x == item => xs
    case x :: xs => x :: pop(item, xs)
  }

  object Input {

    def parse(item: String): Building = {
      val value = item.tail.init.split(",").map(_.toInt)
      Building(value(0), value(2), value(1))
    }

    // (2,3,10);(9,2,10)
    def unapply(line: String) =
      Some(line.replace(" ", "").split(";").map(parse).toList)

  }

}