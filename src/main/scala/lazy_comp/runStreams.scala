object runStreams{
  def main(args: Array[String]): Unit = {
    val stream = Stream(1,2,3,4,5)
    println(stream) //Cons(<function0>,<function0>)
    println(stream.toList) //List(1, 2, 3)
    println(stream.take(3).toList)
    println(stream.drop(2).toList)
  }
}
