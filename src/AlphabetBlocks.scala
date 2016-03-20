/**
  * See <a href="https://www.codeeval.com/open_challenges/201/">Alphabet blocks</a>
  */
object AlphabetBlocks extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines()

  lines.collect {
    case Input(word, blocks) => check(word, blocks)
  } foreach {
    result => println(result.toString.capitalize)
  }

  def check(word: List[Char], blocks: Set[String]): Boolean = word match {
    case Nil => true
    case x :: xs => blocks.exists(i => i.contains(x) && check(xs, blocks - i))
  }

  object Input {

    // 4 | DOG | UPZRHR INOYLC KXDHNQ BAGMZI
    def unapply(line: String) = line.split(" \\| ").toList match {
      case any :: word :: blocks :: Nil => Some(word.toList, blocks.split(" ").toSet)
      case _ => None
    }

  }

}