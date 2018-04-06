import ExpressionMachine.Machine

object Main{
  def main(args: Array[String]): Unit = {

//    //test Less - Exception
//    new Machine().run(Less(Var("x"), Number(5)), Map("x" -> "abc"))
//    println()
//
    //test Less - Exception
//    new Machine().run(Less(Sum(Var("x"), Prod(Number(4),Number(2))), Sum(Number(5),Number(3))), Map("x" -> 1))
//    println()
//
    //test Prod
    new Machine().run(Prod(Sum(Number(3), Number(6)), Prod(Number(3), Sum(Var("x"), Number(5)))),
      Map("x" -> "afa"))
    println()

//    //test prod parentheses
//    new Machine().run(Prod(Sum(Prod(Number(2), Number(3)), Number(4)),Number(2)),Map())
//    println()
//  //test Sum
//    new Machine().run(
//      Prod(Sum(Var("x"), Number(2)), Sum(Number(4), Var("y"))),
//      Map("x" -> 1, "y" -> 2))
//    println()

    //test ifelse
//    new Machine().run(
//      IfElse(Less(Sum(Var("x"), Number(4)),Number(10)), Prod(Number(4),Number(6)), Number(2)),
//      Map("x" -> 1, "y" -> 2))


  }
}