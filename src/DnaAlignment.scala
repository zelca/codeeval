/**
  * See <a href="https://www.codeeval.com/open_challenges/171/">DNA alignment</a>
  *
  * Levenshtein distance.
  */
object DnaAlignment extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.collect {
    case Input(pattern, dna) => dist(pattern, dna)
  } foreach println

  def dist(pattern: List[Char], dna: List[Char]): Int = {

    type FF = Memo[List[Char], List[Char], (Int, Int), (Int, Int, (Int, Int)), Int]

    implicit def key(i: (List[Char], List[Char], (Int, Int))): (Int, Int, (Int, Int)) = (i._1.size, i._2.size, i._3)

    lazy val eval: FF = Memo {
      case (Nil, Nil, p) => 0
      case (Nil, x2 :: xs2, p) => p._1 + eval(Nil, xs2, (-1, -8))
      case (x1 :: xs1, Nil, p) => p._2 + eval(xs1, Nil, (-1, -8))
      case (x1 :: xs1, x2 :: xs2, p) if x1 == x2 =>
        (3 + eval(xs1, xs2, (-8, -8))).max(p._1 + eval(x1 :: xs1, xs2, (-1, -8))).max(p._2 + eval(xs1, x2 :: xs2, (-8, -1)))
      case (x1 :: xs1, x2 :: xs2, p) =>
        (-3 + eval(xs1, xs2, (-8, -8))).max(p._1 + eval(x1 :: xs1, xs2, (-1, -8))).max(p._2 + eval(xs1, x2 :: xs2, (-8, -1)))
    }

    eval(pattern, dna, (-8, -8))
  }

  case class Memo[X, Y, Z, K, R](f: (X, Y, Z) => R)(implicit ev: ((X, Y, Z)) => K) extends ((X, Y, Z) => R) {

    private val cache = scala.collection.mutable.Map.empty[K, R]

    override def apply(x: X, y: Y, z: Z): R = cache.getOrElseUpdate((x, y, z), f(x, y, z))

  }

  object Input {

    // GAAAAAAT | GAAT
    def unapply(line: String) = line.split(" \\| ").toList match {
      case x :: y :: Nil => Some(x.toList, y.toList)
      case _ => None
    }

  }

}