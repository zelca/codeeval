/**
  * See <a href="https://www.codeeval.com/open_challenges/118/">Seat Your Team</a>
  */
object SeatYourTeam extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.collect {
    case Input(favorites, available) =>
      eval(favorites, Set())
  } foreach {
    case true => println("Yes")
    case false => println("No")
  }

  def eval(favorites: List[List[Int]], path: Set[Int]): Boolean =
    favorites match {
      case Nil => true
      case x :: xs => x.exists(p => !path.contains(p) && eval(xs, path + p))
    }

  object Input {

    // 4; 1:[1, 3, 2], 2:[1], 3:[4, 3], 4:[4, 3]
    def unapply(line: String) = line.split("; ").toList match {
      case n :: places :: Nil =>
        val available = 1 to n.toInt
        val favorites = places.split("(\\],|]$)").map(_.split("(:\\[|, )").toList).map(_.tail.map(_.toInt))
        Some(favorites.toList, available.toSet)
      case _ => None
    }

  }

}