/**
  * Adds local file with same name as class if args not specified.
  */
trait Challenge extends App {

  override def main(args: Array[String]) =
    if (args.length > 0)
      super.main(args)
    else {
      val clazz = getClass.getSimpleName.init
      super.main(Array(getClass.getResource(clazz).getPath))
    }

}
