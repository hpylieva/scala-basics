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

    def reductionStep(expr: Expr, env: Map[String, Any]): Expr = expr match {
      case Prod(lOp, rOp) => {
        if (lOp.isReducible) Prod(reductionStep(lOp, env), rOp)
        else if (rOp.isReducible) Prod(lOp, reductionStep(rOp, env))
        else lOp match {
          case Number(lOp) => rOp match {
            case Number(rOp) => rOp match {
              case i: Int => Number(i)
            }
            case _ => throw new CustomException("Exception: Wrong Right Operand type in Product.")
          }
          case _ => throw new CustomException("Exception: Wrong Left Operand type in Product.")
        }
      }

      case Sum(lOp, rOp) => {
        if (lOp.isReducible) Sum(reductionStep(lOp, env), rOp)
        else if (rOp.isReducible) Sum(lOp, reductionStep(rOp, env))
        else lOp match {
          case Number(lOp) => rOp match {
            case Number(rOp) => expr.evaluate
            case _ => throw new CustomException("Exception: Wrong Right Operand type in Sum.")
          }
          case _ => throw new CustomException("Exception: Wrong Left Operand type in Sum.")
        }
      }

      case Var(name) => {
        if (env.contains(name)) env(name) match {
          case i: Int => Number(i)
          case b: Boolean => Bool(b)
          case _ => throw new CustomException("Exception: In this Tiny Language Var cannot be of the type specified. Sorry for inconvenience =)")
        }
        else throw new CustomException("Exception: Var name is not present in Environment.")
      }

      case Less(lOp, rOp) => {
        if (lOp.isReducible) Less(reductionStep(lOp, env), rOp)
        else if (rOp.isReducible) Less(lOp, reductionStep(rOp, env))
        else lOp match {
          case Number(lOp) => rOp match {
            case Number(rOp) => expr.evaluate
            case _ => throw new CustomException("Exception: Wrong Right Operand type in Less.")
          }
          case _ => throw new CustomException("Exception: Wrong Left Operand type in Less.")
        }
      }

    }

  }

}
