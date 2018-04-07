

object Main{
  def main(args: Array[String]): Unit = {

    val runExpressionPart = false
    val runStatementPart = true

    if (runExpressionPart) {
      //test Less - Exception
      new Machine().run(Less(Var("x"), Number(5)), Map("x" -> Number(1)))

      //test Less - Exception
      new Machine().run(Less(Sum(Var("x"), Prod(Number(4), Number(2))), Sum(Number(5), Number(3))), Map("x" -> Number(1)))

      //test Prod
      new Machine().run(Prod(Sum(Prod(Number(3), Number(6)),Number(3)), Prod(Number(3), Sum(Var("x"), Number(5)))),
        Map("x" -> Number(1)))

      //test Sum
      new Machine().run(
        Prod(Sum(Var("x"), Number(2)), Sum(Number(4), Var("y"))),
        Map("x" -> Number(1), "y" -> Number(2)))

      //test ifelse
      new Machine().run(
        IfElse(Less(Sum(Var("x"), Number(4)), Number(10)), Prod(Number(4), Number(6)), Number(2)),
        Map("x" -> Number(1), "y" -> Number(2)))
    }

    if(runStatementPart){
      //  new Machine().run( DoNothing, Map("x" -> Number(1), "y" -> Number(2)))

     val m = new Machine()
      m.run(
        Assign("new_val",Number(5)),
        Map("x" -> Number(1), "y" -> Number(2)))

  //      DoNothing(Number(4))
    }

  }
}