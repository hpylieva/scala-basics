object ExpressionMachine {

  final class Machine {
    def run(expr: Expr, env: Map[String, Any]): Option[Expr] = {
      println(expr)

      if (expr.isReducible) {
        try {
          run(reductionStep(expr, env), env)
        } catch {
          case exception: CustomException => println(exception.msg)
            None
        }
      }
      else
        Option(expr)
    }



    def reductionStep(expr: Expr, env: Map[String, Any]): Expr = {

      def func(applyFunc: (Expr, Expr) => Expr, lOp: Expr, rOp: Expr, expr: Expr): Expr = {
        if (lOp.isReducible) applyFunc(reductionStep(lOp, env), rOp)
        else if (rOp.isReducible) applyFunc(lOp, reductionStep(rOp, env))
        else lOp match {
          case Number(_) => rOp match {
            case Number(_) => expr.evaluate
            case _ => throw CustomException("Exception: Wrong Right Operand type.")
          }
          case _ => throw CustomException("Exception: Wrong Left Operand type.")
        }
      }

      expr match {
        case Prod(lOp, rOp) => func(Prod.apply, lOp, rOp, expr)

        case Sum(lOp, rOp) => func(Sum.apply, lOp, rOp, expr)

        case Var(name) => {
          if (env.contains(name)) env(name) match {
            case i: Int => Number(i)
            case b: Boolean => Bool(b)
            case _ => throw CustomException("Exception: In this Tiny Language Var cannot be of the type specified. Sorry for inconvenience =)")
          }
          else throw CustomException("Exception: Var name is not present in Environment.")
        }

        case Less(lOp, rOp) => func(Less.apply, lOp, rOp, expr)

        case IfElse(conditionExpr, ifExpr, elseExpr) => {
          if (conditionExpr.isReducible) IfElse(reductionStep(conditionExpr, env), ifExpr, elseExpr)
          else conditionExpr match {
            case Bool(b) => if (b) {
              if (ifExpr.isReducible) reductionStep(ifExpr, env)
              else ifExpr.evaluate
            }
            else {
              if (elseExpr.isReducible) reductionStep(elseExpr, env)
              else elseExpr.evaluate
            }
            case _ => throw new Exception("Exception: IfElse condition type is not Bool.")
          }
        }
      }
    }

  }

}
