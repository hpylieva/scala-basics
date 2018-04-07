final class Machine{
  def run(expr: Expr, env: Map[String, Expr]): Option[Expr] = {
    println(expr)

    if (expr.isReducible) {
      try {
        run(reductionStep(expr, env), env)
      } catch {
        case exception: CustomException => println(exception.msg)
          None
          println()
      }
    }
    else
      println()
      Option(expr)
  }

  def reductionStep(expr: Expr, env: Map[String, Expr]): Expr = {

    def reduceBinaryOperation(applyFunc: (Expr, Expr) => Expr, lOp: Expr, rOp: Expr): Expr = {
      if (lOp.isReducible) applyFunc(reductionStep(lOp, env), rOp)
      else if (rOp.isReducible) applyFunc(lOp, reductionStep(rOp, env))
      else expr.evaluate
    }

    expr match {
      case Prod(lOp, rOp) => reduceBinaryOperation(Prod.apply, lOp, rOp)
      case Sum(lOp, rOp) => reduceBinaryOperation(Sum.apply, lOp, rOp)
      case Var(name) => {
        if (env.contains(name)) env(name)
        else throw CustomException("Exception: Var name is not present in Environment.")
      }

      case Less(lOp, rOp) => reduceBinaryOperation(Less.apply, lOp, rOp)

      case IfElse(conditionExpr, ifExpr, elseExpr) => {
        if (conditionExpr.isReducible) IfElse(reductionStep(conditionExpr, env), ifExpr, elseExpr)
        else if (ifExpr.isReducible) reductionStep(ifExpr, env)
        else reductionStep(elseExpr, env)
      }
    }
  }

  def run(statement: Statement, env: Map[String, Any]): Map[String, Any] = {
    println(s"Environment:\n$env\n\nRunning statement:\n$statement\n")

    try {
      execStatement(statement, env)
    } catch {
      case exception: CustomException => env + ("__error" -> exception.msg)
        //None TODO: add Option
    }

  }

  def printResult(env: Map[String, Any]): Unit =  println(s"Environment:\n$env")

  def execStatement(statement: Statement, env: Map[String, Any]): Map[String, Any] = statement match{
    case  DoNothing => env
    case Assign(varName, expr) =>
//      if(expr.isReducible) execAssign(varName, reductionStep(expr, env), env)
//    else
        execAssign(varName, expr, env)
    case Sequence(statementsList) => statementsList.foldLeft(env)((env, s) => run(s, env))
  }

  private def execAssign(name: String, expr: Expr, env: Map[String, Any]):Map[String, Any] = {
    if(env contains name) env - name + (name -> expr.evaluate)
    else env + (name -> expr.evaluate)
  }
}

