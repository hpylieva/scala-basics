import scala.collection.mutable

object Main{
  def main(args: Array[String]): Unit = {

    val runExpressionPart = !false
    val runStatementPart = true

    if (runExpressionPart) {
      //test Less - Exception
      new Machine(Map("x" -> Bool(true))).run(Less(Var("x"), Number(5)))

      //test Less - Exception
      new Machine(Map("x" -> Number(1))).run(Less(Sum(Var("x"),
        Prod(Number(4), Number(2))), Sum(Number(5), Number(3)))
        )

      //test Prod
      new Machine(Map("x" -> Number(1))).run(Prod(Sum(Prod(Number(3), Number(6)),Number(3)),
        Prod(Number(3), Sum(Var("x"), Number(5)))))

      //test Sum
      new Machine(Map("x" -> Number(1), "y" -> Number(2))).run(
        Prod(Sum(Var("x"), Number(2)), Sum(Number(4), Var("y"))))
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

     val m = new Machine(Map("x" -> Number(12), "y" -> Number(2)))
//      m.run(
//        Assign("x",Prod(Sum(Number(2), Number(1)),Sum(Number(4),Number(3))))
//        )
//      m.run(IfElseStatement(Less(Prod(Number(0), Number(4)),Number(5)),
//        Assign("x",Sum(Number(10), Number(2))),
//        Assign("x", Number(32))))
      m.run(WhileLoop(Less(Var("x"),Sum(Number(16), Number(1))),
        Assign("x", Sum(Var("x"), Number(1)))))
      m.run(Sequence(List(Assign("x",Bool(true)), Assign("y", Number(10)))))
//      println("Final state")
//      m.printEnv()

    }

  }
}