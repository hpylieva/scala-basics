object ExpressionMachine {

  final class Machine {
    def run(expr: Expr, env: Map[String, Any]): Expr = {
      println(expr)

      if (expr.isReducible)
        run(reductionStep(expr, env), env)
      else
        expr
    }

    def reductionStep(expr: Expr, env: Map[String, Any]): Expr = expr match {
      case Prod(lOp, rOp) => {
        if (lOp.isReducible) Prod(reductionStep(lOp, env), rOp)
        else if (rOp.isReducible) Prod(lOp, reductionStep(rOp, env))
        else lOp match {
          case Number(lOp) => rOp match {
            case Number(rOp) => rOp match{
              case i:Int => Number(i)
            }
            case _ => throw new CustomException("Wrong Right Operand type in Product exception!")
          }
          case _ => throw new CustomException("Wrong Left Operand type in Product exception!")
        }
      }

      case Sum(lOp, rOp) =>{
        if (lOp.isReducible) Sum(reductionStep(lOp, env), rOp)
        else if (rOp.isReducible) Sum(lOp, reductionStep(rOp, env))
        else lOp match {
          case Number(lOp) => rOp match {
            case Number(rOp) => rOp match{
              case i:Int => Number(i)
            }
            case _ => throw new CustomException("Wrong Right Operand type in Sum exception!")
          }
          case _ => throw new CustomException("Wrong Left Operand type in Sum exception!")
        }
      }

      case Var(name) => {
        if(env.contains(name)) env(name) match {
          case i:Int => Number(i)
          case b:Boolean => Bool(b)
        }
        else throw new CustomException("Var name is not in Environment exception!")
      }

//      case Less(lOp, rOp) =>
//        if (lOp.isReducible) Less(reductionStep(lOp, env), rOp)
//        else if (rOp.isReducible) Sum(lOp, reductionStep(rOp, env))
//        else lOp match {
//          case Number(lOp) => rOp match {
//            case Number(rOp) => Number(expr.eval)
    }

  }


  def main(args: Array[String]): Unit = {

    try {
      new Machine().run(Prod(Sum(Number(2), Number(2)), Number(4)),
        Map("x" -> 1))
      println()
      new Machine().run(
        Prod(Sum(Var("x"), Number(2)), Sum(Number(4), Var("y"))),
        Map("x" -> 1, "y" -> 3))
    }
    catch {
      case ex: CustomException => println(ex.msg)
    }
//    Prod(Sum(Number(2), Var("x")), Var("y"))
//    Prod(Sum(Number(3), Number(6)), Prod(Number(3), Sum(Var("x"), Number(5))))
//    Prod(Sum(Number(3), Number(6)), Var("x"))
  }

}