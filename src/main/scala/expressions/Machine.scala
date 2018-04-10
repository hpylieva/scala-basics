final class Machine(){

  def run(expr: Expr, env:  Map[String, Expr]): Option[Expr] = {
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

  private def reductionStep(expr: Expr, env:  Map[String, Expr]): Expr = {

    def reduceBinaryOperation(applyFunc: (Expr, Expr) => Expr, lOp: Expr, rOp: Expr): Expr = {
      if (lOp.isReducible) applyFunc(reductionStep(lOp, env), rOp)
      else if (rOp.isReducible) applyFunc(lOp, reductionStep(rOp, env))
      else expr.evaluate
    }

    expr match {
      case Number(_) | Bool(_) => expr
      case Prod(lOp, rOp) => reduceBinaryOperation(Prod.apply, lOp, rOp)
      case Sum(lOp, rOp) => reduceBinaryOperation(Sum.apply, lOp, rOp)
      case Var(name) =>
        if (env contains name) env(name)
        else throw CustomException("Exception: Var name is not present in Environment.")

      case Less(lOp, rOp) => reduceBinaryOperation(Less.apply, lOp, rOp)

      case IfElse(conditionExpr, ifExpr, elseExpr) => {
        if (conditionExpr.isReducible) IfElse(reductionStep(conditionExpr, env), ifExpr, elseExpr)
        else
          if (conditionExpr.toBool) reductionStep(ifExpr, env)
          else reductionStep(elseExpr, env)
      }
      case _ => throw CustomException("Exception: This Expression is not supported yet.")
    }
  }


  def printEnv(env:Map[String, Expr]): Unit =
    println("Environment: { "+ env.map{case (k, v) => k + ":" + v}.mkString(" | ") + s" }")



  def run(statement: Statement, env: Map[String, Expr]): Map[String, Expr] = {
    println()
    printEnv(env)
    println("Running statement:\n"+statement)

    try {
      runStatement(statement, env)
    } catch {

      case e: CustomException => {
        println(e.getMessage)
        env + ("__error" -> Str(e.getMessage))
      }

    }
  }

  private def runStatement(statement: Statement, env: Map[String, Expr]): Map[String, Expr] = {
    statement match {
      case DoNothing => env

      case Assign(name, expr) =>
        if(expr.isReducible) assignRun(name, reductionStep(expr, env), env)
        else assignRun(name, expr, env)

      case IfElseStatement(cond, ifSt, elseSt) =>
        if(cond.isReducible) ifElseRun(reductionStep(cond, env), ifSt, elseSt, env)
        else ifElseRun(cond, ifSt, elseSt, env)

      case WhileLoop(cond, statement) => {
        val reduced_cond = if (cond.isReducible) {
          val ex = run(cond, env).getOrElse(Bool(false))
            ex.toBool
        } else cond.toBool
        print(reduced_cond)
        printEnv(env)

        if (reduced_cond) {
          run(WhileLoop(cond, statement), run(statement, env))
        }
        else {
          printEnv(env)
          env
        }
      }

      case Sequence(ls) => ls.foldLeft(env)((env, s) => runStatement(s, env))
    }
  }

  private def whileRun(cond: Expr, statement: Statement, env: Map[String, Expr]):Map[String, Expr] = {
//doesn't work

    //    val reduced_condition = reductionStep(cond, env)
//    print(reduced_condition)
//    reduced_condition match {
//      case Bool(b) => if (b) runStatement (WhileLoop (cond, statement), runStatement (statement, env) )
//      else env
//      case _ => throw new Exception("While loop condition reduced not to Bool type") //TODO: substitute Exception withh CustomException
//    }
    env

  }

  private def ifElseRun(cond: Expr, ifSt: Statement, elseSt: Statement, env: Map[String, Expr]): Map[String, Expr] = {
    if(cond.isReducible) ifElseRun(reductionStep(cond, env), ifSt, elseSt, env)
    else
      if (cond.toBool) runStatement(ifSt, env)
      else runStatement(elseSt, env)
  }

  private def assignRun(name: String, expr: Expr, env: Map[String, Expr]):Map[String, Expr] = {
    if(expr.isReducible) assignRun(name, reductionStep(expr, env), env)
    else {
      printEnv(env + (name -> expr))
      env + (name -> expr)
    }
  }

}

