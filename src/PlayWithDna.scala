/**
  * See <a href="https://www.codeeval.com/open_challenges/126/">Play with DNA</a>
  *
  * Levenshtein distance.
  */
object PlayWithDna extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.collect {
    case Input(pattern, limit, dna) =>
      dna.sliding(pattern.size).map(slice => (dist(pattern, slice.toList), slice)).filter(_._1 <= limit).toList
  } foreach {
    case Nil => println("No match")
    case result => println(result.sorted.map(_._2).mkString(" "))
  }

  def dist(pattern: List[Char], dna: List[Char]): Int = {

    type FF = Memo[List[Char], List[Char], (Int, Int), Int]

    implicit def key(i: (List[Char], List[Char])): (Int, Int) = (i._1.size, i._2.size)

    lazy val eval: FF = Memo {
      case (Nil, s2) => s2.size
      case (s1, Nil) => s1.size
      case (x1 :: xs1, x2 :: xs2) if x1 == x2 => eval(xs1, xs2)
      case (x1 :: xs1, x2 :: xs2) => eval(xs1, xs2).min(eval(x1 :: xs1, xs2)).min(eval(xs1, x2 :: xs2)) + 1
    }

    eval(pattern, dna)
  }

  case class Memo[X, Y, K, R](f: (X, Y) => R)(implicit ev: ((X, Y)) => K) extends ((X, Y) => R) {

    private val cache = scala.collection.mutable.Map.empty[K, R]

    override def apply(x: X, y: Y): R = cache.getOrElseUpdate((x, y), f(x, y))

  }

  object Input {

    // AGTTATC 2 AGTATGC
    def unapply(line: String) = line.split(" ").toList match {
      case pattern :: limit :: dna :: Nil => Some(pattern.toList, limit.toInt, dna)
      case _ => None
    }

  }

}