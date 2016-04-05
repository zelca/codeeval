/**
  * See <a href="https://www.codeeval.com/open_challenges/185/">Glue Shredded Pieces</a>
  */
object GlueShreddedPieces extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.collect {
    case Input(pieces) => find(pieces)
  } foreach println

  def find(pieces: List[String]): String = {
    val last = pieces.find(x => !pieces.exists(x.tail == _.init)).get
    find(pieces diff List(last), List(last)).get
  }

  def find(pieces: List[String], res: List[String]): Option[String] = pieces match {
    case Nil =>
      Some(res.init.foldRight(List(res.last))((x, xs) => x.head.toString :: xs).mkString)
    case xs =>
      val hi = res.head.init
      val matches = xs.filter(_.tail == hi).toSet
      matches.find(x => !xs.exists(x.tail == _.init)) match {
        case Some(x) => find(pieces diff List(x), x :: res)
        case None => matches.map(x => find(pieces diff List(x), x :: res)).find(_.nonEmpty).map(_.get)
      }
  }

  object Input {

    // |deEva|lan t|to ha|evil |
    def unapply(line: String) =
      Some(line.split("\\|").toList.filter(_.nonEmpty))

  }

}