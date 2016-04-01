/**
  * See <a href="https://www.codeeval.com/open_challenges/216/">Everything or Nothing</a>
  */
object EverythingOrNothing extends Challenge {

  val lines = scala.io.Source.fromFile(args(0)).getLines().filter(_.length > 0)

  val rights: Map[(String, String), Set[String]] = Map(
    ("user_1", "file_1") -> Set("read", "write", "grant"),
    ("user_1", "file_2") -> Set("write", "grant"),
    ("user_1", "file_3") -> Set(),
    ("user_2", "file_1") -> Set("read", "write"),
    ("user_2", "file_2") -> Set("write"),
    ("user_2", "file_3") -> Set("read"),
    ("user_3", "file_1") -> Set("read", "grant"),
    ("user_3", "file_2") -> Set("grant"),
    ("user_3", "file_3") -> Set("read", "grant"),
    ("user_4", "file_1") -> Set("write", "grant"),
    ("user_4", "file_2") -> Set("read", "write", "grant"),
    ("user_4", "file_3") -> Set("grant"),
    ("user_5", "file_1") -> Set("read", "write"),
    ("user_5", "file_2") -> Set(),
    ("user_5", "file_3") -> Set("write"),
    ("user_6", "file_1") -> Set("read"),
    ("user_6", "file_2") -> Set("write"),
    ("user_6", "file_3") -> Set("read", "write"))

  lines.collect {
    case Input(actions) => eval(actions, rights)
  } foreach {
    result => println(result.toString.capitalize)
  }

  import scala.annotation.tailrec

  @tailrec
  def eval(actions: List[List[String]], rights: Map[(String, String), Set[String]]): Boolean = actions match {
    case Nil =>
      true
    case (user :: file :: action :: xl) :: xs if !rights(user, file).contains(action) =>
      false
    case (_ :: file :: _ :: action :: user2 :: Nil) :: xs =>
      eval(xs, rights + ((user2, file) -> (rights(user2, file) + action)))
    case _ :: xs =>
      eval(xs, rights)
  }

  object Input {

    // user_1=>file_1=>read user_2=>file_2=>write
    def unapply(line: String) =
      Some(line.split(" ").toList.map(_.split("=>").toList))

  }

}
