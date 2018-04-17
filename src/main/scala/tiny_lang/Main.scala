
object Main{
  def main(args: Array[String]): Unit = {

    val runExpressionPart = true
    val runStatementPart = true

    val machine = new Machine
    val env = Map("x" -> 3, "y" -> 5, "b" ->true) // <= initial environment for the tests

    def reductionStep(expr: Expr): Expr =
      machine.reductionStep(expr, env)
    def reduce(expr: Expr): Option[Expr] =
      machine.reduce(expr, env)
    def run(stat: Stat): Map[String, Any] =
      machine.run(stat, env)

    if (runExpressionPart) {
      println("-------------Running EXPRESSION part-------------")
      reduce(Number(5))
      reduce(Var("b"))
      reduce(Var("unknown"))

      // Sum
      //      test("Sum of two Numbers reduces to Number with their sum")
      reduce(Sum(Number(5), Number(10)))
      //      test("Sum of Number and Bool does not reduce")
      reduce(Sum(Number(5), Bool(true)))
      //      test("Sum of Bool and Number does not reduce")
      reduce(Sum(Bool(false), Number(10)))
      //      test("left Sum operand reduces if it is reducible and right is left unchanged")
      reduce(Sum(Prod(Number(10), Var("x")), Number(10)))
      //      test("otherwise right Sum operand reduces")
      reduce(Sum(Number(10), Prod(Number(10), Var("x"))))
      //      test complex expression
      reduce(Sum(Sum(Var("x"), Number(2)), Sum(Number(4), Var("y"))))

      // Prod
      //      test("Prod of two Numbers reduces to Number with their sum")
      reduce(Prod(Number(5), Number(10)))
      //      test("Prod of Number and Bool does not reduce")
      reduce(Prod(Number(5), Bool(true)))
      //      test("Prod of Bool and Number does not reduce")
      reduce(Prod(Bool(false), Number(10)))
      //      test("left Prod operand reduces if it is reducible and right is left unchanged")
      reduce(Prod(Sum(Number(10), Var("x")), Number(10)))
      //      test("otherwise right Prod operand reduces")
      reduce(Prod(Number(10), Sum(Number(10), Var("x"))))
      //      test complex expression
      reduce(Prod(Sum(Prod(Number(3), Number(6)),Number(3)),
              Prod(Number(3), Sum(Var("x"), Number(5)))))

      // Less
      //      test("Less of two Numbers reduces to Number with their sum")
      reduce(Less(Number(5), Number(10)))
      //      test("Less of Number and Bool does not reduce")
      reduce(Less(Number(5), Bool(true)))
      //      test("Less of Bool and Number does not reduce")
      reduce(Less(Bool(false), Number(10)))
      //      test("left Less operand reduces if it is reducible and right is left unchanged")
      reduce(Less(Sum(Number(10), Var("x")), Number(10)))
      //      test("otherwise right Less operand reduces")
      reduce(Less(Number(10), Sum(Number(10), Var("x"))))

      //      test complicated Less
      reduce(Less(Sum(Var("x"),Prod(Var("x"), Number(2))), Sum(Number(5), Number(3))))

      // IfElse
      //      test("IfElse reduces to thenExpr for Bool(true) condition")
      reduce(IfElse(Less(Var("x"), Number(4)), Number(1), Number(2)))
      //      test("IfElse reduces to elseExpr for Bool(false) condition")
      reduce(IfElse(Less(Number(8), Number(4)), Number(1), Number(2)))
        //      test("IfElse for Number condition does not reduce")
      reduce(IfElse(Number(3), Number(1), Number(2)))
      //      test("IfElse for reducible condition reduces its condition")
      reduce( IfElse(Less(Sum(Var("x"), Number(4)), Number(1)),
                Prod(Number(4), Number(6)), Number(2)))

    }

    if(runStatementPart){
      println("-------------Running STATEMENT part-------------")
      // DoNothing
      //      test("DoNothing does not alter environment")
      run(DoNothing)

      // Assign
      //      test("Assign adds new variable for number expression")
      run(Assign("t",Number(0)))
      //      test("Assign updates existing variable for boolean expression")
      run(Assign("y",Bool(true)))
      //      test("Assign updates existing variable for expression with the same variable")
      run(Assign("y",Var("y")))
      run(Assign("y",Var("x")))
      //      test("Assign does not occur for erroneous expression")
      run(Assign("y",Var("z")))

      // If
      //      test("'If' runs thenStat if condition is Bool(true)")
      run(IfElseStat(Less(Var("y"), Number(4)),Assign("y",Bool(true)),Assign("t",Bool(true)) ))
      //      test("'If' runs elseStat if condition is Bool(false)")
      run(IfElseStat(Less(Var("y"), Number(1)),Assign("y",Bool(true)),Assign("t",Bool(true)) ))
      //      test("'If' statement fails for erroneous condition")
      run(IfElseStat(Less(Var("z"), Number(1)),Assign("y",Bool(true)),Assign("t",Bool(true)) ))
      //      test("'If' statement fails for condition expression that reduces to Number")
      run(IfElseStat( Number(1),Assign("y",Bool(true)),Assign("t",Bool(true)) ))

      // Seq
      //      test("'Seq' does nothing if empty")
      run(Sequence(List()))
      //      test("'Seq' executes one its statement if contains only one")
      run(Sequence(List(Assign("t",Bool(true)))))
      //      test("'Seq' executes its statements one by one")
      run(Sequence(List(
                Assign("x",Bool(true)),
                Assign("y", Number(10)),
                IfElseStat(Bool(true), Assign("t",Bool(false)),  Assign("t", Number(14))))))
      //      test("'Seq' does not execute remained statements after first failure")
      run(Sequence(List(
        Assign("x",Bool(true)),
        Assign("z", Var("t")),
        IfElseStat(Bool(true), Assign("t",Bool(false)),  Assign("t", Number(14))))))

      // While
      //      test("'While' executes thenStat multiple times while condition reduces to Bool(true)")
      run(While(
        Less(Sum(Var("x"),Number(1)), Number(7)),
        Assign("x", Sum(Var("x"), Number(1)))))
      //      test("'While' does not execute thenStat if condition reduces to Bool(false) from the start")
      //      test("'While' statement fails for erroneous condition")
      run(While(
        Less(Sum(Var("t"),Number(1)), Number(7)),
        Assign("x", Sum(Var("x"), Number(1)))))
      //      test("'While' statement fails for condition expression that reduces to Number")
      //      test("'While' statement fails if thenStat statement fails")
      run(While(
        Less(Sum(Var("x"),Number(1)), Number(7)),
        Assign("x", Sum(Var("t"), Number(1)))))

      // sophisticated WhileLoop
//      run(While(Less(Sum(Var("x"),Number(4)),Prod(Sum(Number(9), Number(11)),Number(1))),
//        Assign("x", Sum(Var("x"), Number(1)))))


    }

  }
}