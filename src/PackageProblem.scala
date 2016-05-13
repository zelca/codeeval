/**
  * See <a href="https://www.codeeval.com/open_challenges/114/">Package Problem</a>
  */
object PackageProblem extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  type Item = (Int, Double, Int)

  lines.collect {
    case Input(weight, items) =>
      val all = eval(items, weight)
      all.maxBy(i => (i.map(_._3).sum, -i.map(_._2).sum))
  } foreach {
    case Nil => println("-")
    case result => println(result.map(_._1).mkString(","))
  }

  def eval(items: List[Item], weight: Double): List[List[Item]] = items match {
    case Nil => Nil :: Nil
    case x :: xs if x._2 <= weight => eval(xs, weight - x._2).map(x :: _) ++ eval(xs, weight)
    case _ :: xs => eval(xs, weight)
  }

  object Input {

    def item(i: String) = i.replaceAll("[()$]", "").split(",").toList match {
      case id :: weight :: price :: Nil => (id.toInt, weight.toDouble, price.toInt)
      case _ => throw new IllegalArgumentException
    }

    // 8 : (1,15.3,$34)
    def unapply(line: String) = line.split(" : ").toList match {
      case weight :: data :: Nil =>
        val items = data.split(" ").map(item)
        Some(weight.toInt, items.toList)
      case _ => None
    }

  }

}