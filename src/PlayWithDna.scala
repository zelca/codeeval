/**
  * See <a href="https://www.codeeval.com/open_challenges/126/">Play with DNA</a>
  *
  * Levenshtein distance.
  */
object PlayWithDna extends App {

  val file =
    if (args.length > 0)
      args(0)
    else {
      val file = getClass.getSimpleName.init
      getClass.getResource(file).getPath
    }

  val lines = scala.io.Source.fromFile(file).getLines()

  lines.collect {
    case PlayWithDnaInput(input) => input
  } map {
    case (pattern, limit, dna) =>
      dna.sliding(pattern.size).map(slice => (dist(pattern, slice.toList), slice)).filter(_._1 <= limit).toList
  } foreach {
    case Nil => println("No match")
    case result => println(result.sorted.map(_._2).mkString(" "))
  }

  def dist(pattern: List[Char], dna: List[Char]): Int = {

    type FF = Memo[List[Char], List[Char], (Int, Int), Int]

    implicit def key(i: (List[Char], List[Char])): (Int, Int) = (i._1.size, i._2.size)

    lazy val mDist: FF = Memo {
      case (Nil, s2) => s2.size
      case (s1, Nil) => s1.size
      case (x1 :: xs1, x2 :: xs2) if x1 == x2 => mDist(xs1, xs2)
      case (x1 :: xs1, x2 :: xs2) => mDist(xs1, xs2).min(mDist(x1 :: xs1, xs2)).min(mDist(xs1, x2 :: xs2)) + 1
    }

    mDist(pattern, dna)
  }

  case class Memo[X, Y, K, R](f: (X, Y) => R)(implicit ev: ((X, Y)) => K) extends ((X, Y) => R) {

    import scala.collection.mutable

    private val cache = mutable.Map.empty[K, R]

    override def apply(x: X, y: Y): R = cache.getOrElseUpdate((x, y), f(x, y))

  }

}

object PlayWithDnaInput {

  // AGTTATC 2 AGTATGC
  def unapply(line: String) = line.split(" ").toList match {
    case pattern :: limit :: dna :: Nil => Some(pattern.toList, limit.toInt, dna)
    case _ => None
  }

}