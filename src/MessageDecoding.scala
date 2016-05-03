/**
  * See <a href="https://www.codeeval.com/open_challenges/36/">Message Decoding</a>
  */
object MessageDecoding extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.collect {
    case Input(header, mapping) => eval(header, -1, mapping)
  } foreach println

  import java.lang.Integer.parseInt

  def eval(header: List[Char], idx: Int, mapping: Map[String, Char]): String =
    header match {
      case '0' :: '0' :: '0' :: Nil =>
        ""
      case xs if idx == -1 =>
        eval(xs.drop(3), parseInt(xs.take(3).mkString, 2), mapping)
      case xs if xs.take(idx) == Array.fill(idx)('1').toList =>
        eval(xs.drop(idx), -1, mapping)
      case xs =>
        mapping(xs.take(idx).mkString) + eval(xs.drop(idx), idx, mapping)
    }

  object Input {

    def parse(message: List[Char], i: Int, n: Int): Map[String, Char] =
      message match {
        case Nil => Map()
        case x :: xs if i == scala.math.pow(2, n) - 1 => parse(x :: xs, 0, n + 1)
        case x :: xs => parse(xs, i + 1, n) + (toBinary(i, n) -> x)
      }

    def toBinary(int: Int, digits: Int) =
      String.format("%" + digits + "s", int.toBinaryString).replace(' ', '0')

    // $#**\0100000101101100011100101000
    def unapply(line: String) = line.partition(_.isDigit) match {
      case (header, message) => Some(header.toList, parse(message.toList, 0, 1))
    }

  }

}
