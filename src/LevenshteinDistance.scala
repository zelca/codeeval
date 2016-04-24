/**
  * See <a href="https://www.codeeval.com/open_challenges/58/">Levenshtein Distance</a>
  */
object LevenshteinDistance extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  lines.span(_ != "END OF INPUT") match {
    case (words, rest) =>
      val all = rest.toList.tail.toSet
      words.map(findFriends(_, all).size).foreach(println)
  }

  def findFriends(word: String, all: Set[String]): Set[String] =
    all.partition(x => isFriend(word.toList, x.toList, 1)) match {
      case (friend, rest) => friend ++ friend.flatMap(x => findFriends(x, rest))
    }

  def isFriend(word: List[Char], friend: List[Char], distance: Int): Boolean = (word, friend) match {
    case (Nil, s2) =>
      s2.size <= distance
    case (s1, Nil) =>
      s1.size <= distance
    case (x1 :: xs1, x2 :: xs2) if x1 == x2 =>
      isFriend(xs1, xs2, distance)
    case (x1 :: xs1, x2 :: xs2) if distance > 0 =>
      isFriend(xs1, xs2, distance - 1) || isFriend(x1 :: xs1, xs2, distance - 1) || isFriend(xs1, x2 :: xs2, distance - 1)
    case _ =>
      false
  }

}