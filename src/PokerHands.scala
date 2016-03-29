/**
  * See <a href="https://www.codeeval.com/open_challenges/86/">Poker hands</a>
  */
object PokerHands extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  // The cards are valued in the order:
  // 2, 3, 4, 5, 6, 7, 8, 9, Ten, Jack, Queen, King, Ace.
  // 1  2  3  4  5  6  7  8    9    10     11    12   13

  val values = Map('2' -> 1, '3' -> 2, '4' -> 3,
    '5' -> 4, '6' -> 5, '7' -> 6, '8' -> 7, '9' -> 8,
    'T' -> 9, 'J' -> 10, 'Q' -> 11, 'K' -> 12, 'A' -> 13)

  case class Card(value: Int, suit: Char) extends Ordered[Card] {
    override def compare(that: Card): Int =
      if (value == that.value) that.suit - suit else that.value - value
  }

  lines.collect {
    case Input(left, right) => compare(eval(left), eval(right))
  } foreach println

  type Value = List[Int]

  def compare: (Value, Value) => String = {
    case (Nil, Nil) => "none"
    case (l :: ls, r :: rs) if l > r => "left"
    case (l :: ls, r :: rs) if l < r => "right"
    case (l :: ls, r :: rs) => compare(ls, rs)
    case _ => throw new UnsupportedOperationException
  }

  def eval =
    royalFlush orElse straightFlush orElse fourKind orElse fullHouse orElse
      flush orElse straight orElse threeKind orElse twoPairs orElse onePair orElse highCard

  //  1 - High Card: Highest value card.
  //  2 - One Pair: Two cards of the same value.
  //  3 - Two Pairs: Two different pairs.
  //  4 - Three of a Kind: Three cards of the same value.
  //  5 - Straight: All cards are consecutive values.
  //  6 - Flush: All cards of the same suit.
  //  7 - Full House: Three of a kind and a pair.
  //  8 - Four of a Kind: Four cards of the same value.
  //  9 - Straight Flush: All cards are consecutive values of same suit.
  // 10 - Royal Flush: Ten, Jack, Queen, King, Ace of same suit.

  def royalFlush = new PartialFunction[List[Card], Value] {
    override def isDefinedAt(cards: List[Card]): Boolean =
      sameSuit(cards).size == 5 && consecutive(cards, 13).size == 5

    override def apply(cards: List[Card]): Value =
      10 :: cards.head.value :: Nil
  }

  def straightFlush = new PartialFunction[List[Card], Value] {
    override def isDefinedAt(cards: List[Card]): Boolean =
      sameSuit(cards).size == 5 && consecutive(cards, cards.head.value).size == 5

    override def apply(cards: List[Card]): Value =
      9 :: cards.head.value :: Nil
  }

  def fourKind = new PartialFunction[List[Card], Value] {
    override def isDefinedAt(cards: List[Card]): Boolean =
      sameValue(cards).size == 4

    override def apply(cards: List[Card]): Value = {
      val four = sameValue(cards)
      val rest = cards diff four
      8 :: four.head.value :: rest.map(_.value)
    }
  }

  def fullHouse = new PartialFunction[List[Card], Value] {
    override def isDefinedAt(cards: List[Card]): Boolean = {
      val three = sameValue(cards)
      val pair = sameValue(cards diff three)
      three.size == 3 && pair.size == 2
    }

    override def apply(cards: List[Card]): Value = {
      val three = sameValue(cards)
      val pair = sameValue(cards diff three)
      7 :: three.head.value :: pair.head.value :: Nil
    }
  }

  def flush = new PartialFunction[List[Card], Value] {
    override def isDefinedAt(cards: List[Card]): Boolean =
      sameSuit(cards).size == 5

    override def apply(cards: List[Card]): Value =
      6 :: cards.map(_.value)
  }

  def straight = new PartialFunction[List[Card], Value] {
    override def isDefinedAt(cards: List[Card]): Boolean =
      consecutive(cards, cards.head.value).size == 5

    override def apply(cards: List[Card]): Value =
      5 :: cards.head.value :: Nil
  }

  def threeKind = new PartialFunction[List[Card], Value] {
    override def isDefinedAt(cards: List[Card]): Boolean =
      sameValue(cards).size == 3

    override def apply(cards: List[Card]): Value = {
      val three = sameValue(cards)
      val rest = cards diff three
      4 :: three.head.value :: rest.map(_.value)
    }
  }

  def twoPairs = new PartialFunction[List[Card], Value] {
    override def isDefinedAt(cards: List[Card]): Boolean = {
      val high = sameValue(cards)
      val low = sameValue(cards diff high)
      high.size == 2 && low.size == 2
    }

    override def apply(cards: List[Card]): Value = {
      val high = sameValue(cards)
      val low = sameValue(cards diff high)
      val rest = (cards diff high) diff low
      3 :: high.head.value :: low.head.value :: rest.map(_.value)
    }
  }

  def onePair = new PartialFunction[List[Card], Value] {
    override def isDefinedAt(cards: List[Card]): Boolean =
      sameValue(cards).size == 2

    override def apply(cards: List[Card]): Value = {
      val pair = sameValue(cards)
      val rest = cards diff pair
      2 :: pair.head.value :: rest.map(_.value)
    }
  }

  def highCard = new PartialFunction[List[Card], Value] {
    override def isDefinedAt(cards: List[Card]): Boolean =
      true

    override def apply(cards: List[Card]): Value = {
      1 :: cards.map(_.value)
    }
  }

  def sameSuit(cards: List[Card]): List[Card] =
    cards.groupBy(_.suit).values.maxBy(_.size)

  def sameValue(cards: List[Card]): List[Card] =
    cards.groupBy(_.value).toList.sortWith((a, b) => if (a._2.size == b._2.size) a._1 > b._1 else a._2.size > b._2.size).head._2

  def consecutive(cards: List[Card], value: Int): List[Card] = cards match {
    case c :: xs if c.value == value => c :: consecutive(xs, value - 1)
    case _ => Nil
  }

  object Input {

    // 6D 7H AH 7S QC 6H 2D TD JD AS
    def unapply(line: String) = line.split(" ").toList match {
      case cards if cards.size == 10 =>
        val hands = cards.map(i => Card(values(i.head), i.last)).grouped(5).toList
        Some(hands.head.sorted, hands.last.sorted)
      case _ => None
    }

  }

}