

object Main{
  def main(args: Array[String]): Unit = {

    val runExpressionPart = true
    val runStatementPart = true

    if (runExpressionPart) {
      //test Less - Exception
      new Machine(Map("x" -> Bool(true))).run(Less(Var("x"), Number(5)))
      println()
      //test Less - Exception
      new Machine(Map("x" -> Number(1))).run(Less(Sum(Var("x"),
        Prod(Number(4), Number(2))), Sum(Number(5), Number(3)))
        )
      println()
      //test Prod
      new Machine(Map("x" -> Number(1))).run(Prod(Sum(Prod(Number(3), Number(6)),Number(3)),
        Prod(Number(3), Sum(Var("x"), Number(5)))))
      println()
      //test Sum
      new Machine(Map("x" -> Number(1), "y" -> Number(2))).run(
        Prod(Sum(Var("x"), Number(2)), Sum(Number(4), Var("y"))))
      println()
      new Machine(
        Map("x" -> Number(1), "y" -> Number(2))).run(
        Sum(Number(4), Var("y")))
      println()
      //test IfElse
      new Machine(Map("x" -> Number(1), "y" -> Number(2))).run(
        IfElse(Less(Sum(Var("x"), Number(4)), Number(1)),
          Prod(Number(4), Number(6)), Number(2))
        )
    }

    if(runStatementPart){

     val m = new Machine(Map("x" -> Number(4), "y" -> Number(2)))
      // simple Assign
      m.run(Assign("y",Prod(Sum(Number(2), Number(1)),Prod(Number(2),Number(3)))))
     // IfElse
      m.run(IfElseStatement(Less(Prod(Number(0), Number(4)),Number(5)),
        Assign("x",Sum(Number(10), Number(2))),
        Assign("x", Number(14))))
      // simple WhileLoop
      m.run(WhileLoop(Less(Var("y"),Number(12)),
        Assign("y", Sum(Var("y"), Number(2)))))
      // sophisticated WhileLoop
      m.run(WhileLoop(Less(Sum(Var("x"),Number(4)),Prod(Sum(Number(9), Number(11)),Number(1))),
        Assign("x", Sum(Var("x"), Number(1)))))
      //sequence
      m.run(Sequence(List(
        Assign("x",Bool(true)),
        Assign("y", Number(10)),
        IfElseStatement(Bool(true), Assign("t",Bool(false)),  Assign("t", Number(14))))))

    }

  }
}