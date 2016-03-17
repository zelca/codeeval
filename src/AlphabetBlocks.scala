/**
  * See <a href="https://www.codeeval.com/open_challenges/201/">Alphabet Blocks</a>
  */
object AlphabetBlocks extends App {

  val file =
    if (args.length > 0)
      args(0)
    else {
      val file = getClass.getSimpleName.init
      getClass.getResource(file).getPath
    }

  val lines = scala.io.Source.fromFile(file).getLines()

  lines.collect {
    case AlphabetBlocksInput(input) => input
  } map {
    input => check(input._1, input._2)
  } foreach {
    result => println(result.toString.capitalize)
  }

  def check(word: List[Char], blocks: Set[String]): Boolean = word match {
    case Nil => true
    case x :: xs => blocks.exists(i => i.contains(x) && check(xs, blocks - i))
  }

}

object AlphabetBlocksInput {

  // 4 | DOG | UPZRHR INOYLC KXDHNQ BAGMZI
  def unapply(line: String) = line.split(" \\| ").toList match {
    case any :: word :: blocks :: Nil => Some(word.toList, blocks.split(" ").toSet)
    case _ => None
  }

}