/**
  * Adds local file with same name as class if args not specified.
  */
trait Challenge extends App {

  val runtime = Runtime.getRuntime

  override def main(args: Array[String]) = {
    val startedTime = System.currentTimeMillis()
    val startedMemory = runtime.freeMemory()
    if (args.length > 0)
      super.main(args)
    else {
      val clazz = getClass.getSimpleName.init
      super.main(Array(getClass.getResource(clazz).getPath))
    }
    val finishedAt = System.currentTimeMillis()
    val finishedMemory = runtime.freeMemory()
    println("Execution time: %s ms.".format(finishedAt - startedTime))
    println("Memory consumed: %skB.".format((startedMemory - finishedMemory) / 1024))
  }

}
