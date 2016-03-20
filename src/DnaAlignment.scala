/**
  * See <a href="https://www.codeeval.com/open_challenges/171/">DNA alignment</a>
  *
  * Levenshtein distance.
  */
object DnaAlignment extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines()

  lines.collect {
    case Input(pattern, dna) => dist(pattern, dna)
  } foreach println

  def dist(pattern: List[Char], dna: List[Char]): Int = {

    type FF = Memo[List[Char], List[Char], (Int, Int), (Int, Int, (Int, Int)), Int]

    implicit def key(i: (List[Char], List[Char], (Int, Int))): (Int, Int, (Int, Int)) = (i._1.size, i._2.size, i._3)

    lazy val mDist: FF = Memo {
      case (Nil, Nil, p) => 0
      case (Nil, x2 :: xs2, p) => p._1 + mDist(Nil, xs2, (-1, -8))
      case (x1 :: xs1, Nil, p) => p._2 + mDist(xs1, Nil, (-1, -8))
      case (x1 :: xs1, x2 :: xs2, p) if x1 == x2 =>
        (3 + mDist(xs1, xs2, (-8, -8))).max(p._1 + mDist(x1 :: xs1, xs2, (-1, -8))).max(p._2 + mDist(xs1, x2 :: xs2, (-8, -1)))
      case (x1 :: xs1, x2 :: xs2, p) =>
        (-3 + mDist(xs1, xs2, (-8, -8))).max(p._1 + mDist(x1 :: xs1, xs2, (-1, -8))).max(p._2 + mDist(xs1, x2 :: xs2, (-8, -1)))
    }

    mDist(pattern, dna, (-8, -8))
  }

  case class Memo[X, Y, Z, K, R](f: (X, Y, Z) => R)(implicit ev: ((X, Y, Z)) => K) extends ((X, Y, Z) => R) {

    import scala.collection.mutable

    private val cache = mutable.Map.empty[K, R]

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