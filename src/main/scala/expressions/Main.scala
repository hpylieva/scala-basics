import scala.collection.mutable

object Main{
  def main(args: Array[String]): Unit = {

    val runExpressionPart = !false
    val runStatementPart = true

    if (runExpressionPart) {
      //test Less - Exception
//      new Machine().run(Less(Var("x"), Number(5)), mutable.Map("x" -> Number(1)))
//
//      //test Less - Exception
//      new Machine().run(Less(Sum(Var("x"), Prod(Number(4), Number(2))), Sum(Number(5), Number(3))),
//        mutable.Map("x" -> Number(1)))
//
//      //test Prod
//      new Machine().run(Prod(Sum(Prod(Number(3), Number(6)),Number(3)), Prod(Number(3), Sum(Var("x"), Number(5)))),
//        mutable.Map("x" -> Number(1)))

      //test Sum
//      new Machine().run(
//        Prod(Sum(Var("x"), Number(2)), Sum(Number(4), Var("y"))),
//        mutable.Map("x" -> Number(1), "y" -> Number(2)))
      new Machine(
        Map("x" -> Number(1), "y" -> Number(2))).run(
        Sum(Number(4), Var("y")))
//
      //test ifelse
      new Machine(Map("x" -> Number(1), "y" -> Number(2))).run(
        IfElse(Less(Sum(Var("x"), Number(4)), Number(1)),
          Prod(Number(4), Number(6)), Number(2))
        )
    }

    if(runStatementPart){
      //  new Machine().run( DoNothing, Map("x" -> Number(1), "y" -> Number(2)))

     val m = new Machine(Map("x" -> Number(1), "y" -> Number(2)))
      m.run(
        Assign("x",Prod(Sum(Number(2), Number(1)),Sum(Number(4),Number(3))))
        )
      m.run(IfElseStatement(Less(Sum(Number(3), Number(4)),Number(5)),
        Assign("x",Number(10)),
        Assign("x", Number(32))))
//      println("Final state")
//      m.printEnv()

  //      DoNothing(Number(4))
    }

  }
}