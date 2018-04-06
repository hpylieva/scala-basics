import ExpressionMachine.Machine

object main{
  def main(args: Array[String]): Unit = {

      //test Less - Exception
      new Machine().run(Less(Var("x"), Number(5)), Map("x" -> "lol"))
      println()

    //test Less - Exception
    new Machine().run(Less(Var("x"), Number(5)), Map("x" -> 10))
    println()

      //test Prod
      new Machine().run(Prod(Sum(Number(2), Number(2)), Number(4)),
        Map("x" -> 1))
      println()

    //test Sum
      new Machine().run(
        Prod(Sum(Var("x"), Number(2)), Sum(Number(4), Var("y"))),
        Map("x" -> 1, "y" -> 2))
    //    Prod(Sum(Number(2), Var("x")), Var("y"))
    //    Prod(Sum(Number(3), Number(6)), Prod(Number(3), Sum(Var("x"), Number(5))))
    //    Prod(Sum(Number(3), Number(6)), Var("x"))

  }
}