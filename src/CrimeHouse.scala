/**
  * See <a href="https://www.codeeval.com/open_challenges/195/">Crime House</a>
  */
object CrimeHouse extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.collect {
    case Input(steps) => eval(steps, Set(), Set(), 0, 0, 0)
  }.foreach(result => println(result.getOrElse("CRIME TIME")))

  import scala.annotation.tailrec

  @tailrec
  def eval(m: List[(String, Int)], in: Set[Int], out: Set[Int], inUnknown: Int, outUnknown: Int, total: Int): Option[Int] = m match {
    case Nil => Some(total)
    case ("L", x) :: xs => x match {
      case 0 => eval(xs, in, out, inUnknown, outUnknown + 1, (total - 1).max(0))
      case i if out.contains(i) && inUnknown > 0 => eval(xs, in, out, inUnknown - 1, outUnknown, (total - 1).max(0))
      case i if out.contains(i) => None
      case i if in.contains(i) => eval(xs, in - i, out + i, inUnknown, outUnknown, (total - 1).max(0))
      case i => eval(xs, in, out + i, inUnknown, outUnknown, (total - 1).max(0))
    }
    case ("E", x) :: xs => x match {
      case 0 => eval(xs, in, out, inUnknown + 1, outUnknown, total + 1)
      case i if in.contains(i) && outUnknown > 0 => eval(xs, in, out, inUnknown, outUnknown - 1, total + 1)
      case i if in.contains(i) => None
      case i if out.contains(i) => eval(xs, in + i, out - i, inUnknown, outUnknown, total + 1)
      case i => eval(xs, in + i, out, inUnknown, outUnknown, total + 1)
    }
    case _ => throw new IllegalArgumentException("Unexpected input")
  }

  object Input {

    // 3; E 5|L 0|E 5
    def unapply(line: String) = line.split(";").toList match {
      case count :: records :: Nil =>
        val result = records.trim.split("\\|").map(_.split(" ")).map(record => (record(0), record(1).toInt))
        Some(result.toList)
      case _ => None
    }

  }

}