/**
  * See <a href="https://www.codeeval.com/open_challenges/155/">ASCII decryption</a>
  *
  * Dirty solution by assuming that space is the most common character.
  */
object AsciiDecryption extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.collect {
    case Input(numbers) =>
      val delta = numbers.groupBy(x => x).maxBy(_._2.length)._1 - 32
      numbers.map(i => (i - delta).toChar)
  } foreach {
    result => println(result.mkString)
  }

  object Input {

    // 5 | s | 92 112 109 40 118 109 109 108 123
    def unapply(line: String) = line.split(" \\| ").toList match {
      case any :: char :: numbers :: Nil => Some(numbers.split(" ").map(_.toInt))
      case _ => None
    }

  }

}