import scala.collection.mutable

object Main{
  def main(args: Array[String]): Unit = {

    val runExpressionPart = !true
    val runStatementPart = true

    if (runExpressionPart) {
      //test Less - Exception
      new Machine().run(Less(Var("x"), Number(5)),Map("x" -> Bool(true)))

      //test Less - Exception
      new Machine().run(Less(Sum(Var("x"),
        Prod(Number(4), Number(2))), Sum(Number(5), Number(3))),
        Map("x" -> Number(1))
        )

      //test Prod
      new Machine().run(Prod(Sum(Prod(Number(3), Number(6)),Number(3)),
        Prod(Number(3), Sum(Var("x"), Number(5)))), Map("x" -> Number(1)))

      //test Sum
      new Machine().run(
        Prod(Sum(Var("x"), Number(2)), Sum(Number(4), Var("y"))),
        Map("x" -> Number(1), "y" -> Number(2)))

      new Machine().run( Sum(Number(4), Var("y")), Map("y" -> Number(1)))

      //test IfElse
      new Machine().run(
        IfElse(Less(Sum(Var("x"), Number(4)), Number(1)),
          Prod(Number(4), Number(6)), Number(2)), Map("x" -> Number(1)))
    }

    if(runStatementPart){

     val m = new Machine()
      // simple Assign
//      m.run(Assign("y",Prod(Sum(Number(5), Number(1)),Number(1))),
//          Map("x" -> Number(4), "y" -> Number(2)))
//      m.run(Assign("y",Sum(Number(5), Number(6))),
//        Map("x" -> Number(4), "y" -> Number(2)))
//      m.run(Assign("y",Prod(Sum(Number(2), Number(1)),Prod(Number(2),Number(3)))),
//        Map("x" -> Number(4), "y" -> Number(2)))
//     // IfElse
//      m.run(IfElseStatement(Less(Prod(Number(1), Number(4)),Sum(Number(5),Number(-3))),
//                            Assign("x",Sum(Number(10), Number(2))),
//                            Assign("x", Number(14))),
//        Map("x" -> Number(4), "y" -> Number(2)))
      // simple WhileLoop
      m.run(WhileLoop(Less(Var("y"),Number(12)),
        Assign("y", Sum(Var("y"), Number(2)))),
        Map("x" -> Number(4), "y" -> Number(8)))
//      // sophisticated WhileLoop
//      m.run(WhileLoop(Less(Sum(Var("x"),Number(4)),Prod(Sum(Number(9), Number(11)),Number(1))),
//        Assign("x", Sum(Var("x"), Number(1)))),
//        Map("x" -> Number(4), "y" -> Number(2)))
      //sequence
//      m.run(Sequence(List(
//        Assign("x",Bool(true)),
//        Assign("y", Number(10)),
//        IfElseStatement(Bool(true), Assign("t",Bool(false)),  Assign("t", Number(14))))),
//        Map("x" -> Number(4), "y" -> Number(2)))

    }

  }
}