final class Machine(){

  def reduce(expr: Expr, env:  Map[String, Any]): Option[Expr] = {
    println(expr)

    if (expr.isReducible) {
      try {
        reduce(reductionStep(expr, env), env)
      } catch {
        case e: CustomException => println(e.msg)
        //  println()
          None
      }
    }
    else {
    //  println()
      Option(expr)
    }
  }

  def reductionStep(expr: Expr, env:  Map[String, Any]): Expr = {
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
        if (env contains name) env(name) match{
          case i:Int => Number(i)
          case b:Boolean => Bool(b)
          case _ => throw CustomException(s"Exception: Value type ${env(name).getClass()} is not supported.")
        }
        else throw CustomException(s"Exception: Variable $name is not defined in the environment.")

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

  def printEnv(env:Map[String, Any]): Unit =
    println("Environment: { "+ env.map{case (k, v) => k + ":" + v}.mkString(" | ") + s" }")

  def run(statement: Stat, env: Map[String, Any]): Map[String, Any] = {
    println()
    printEnv(env)
    println("Running statement:\n"+statement)

    try {
      runStatement(statement, env)
    } catch {
      case e: CustomException => {
        println(e.msg)
        env + ("__error" -> e.getMessage)
      }
    }
  }

  private def runStatement(statement: Stat, env: Map[String, Any]): Map[String, Any] = {
    statement match {
      case DoNothing => env

      case Assign(name, expr) =>
        if(expr.isReducible) assignRun(name, reductionStep(expr, env), env)
        else assignRun(name, expr, env)

      case IfElseStat(cond, ifSt, elseSt) =>
        if(cond.isReducible) ifElseRun(reductionStep(cond, env), ifSt, elseSt, env)
        else ifElseRun(cond, ifSt, elseSt, env)

      case While(condition, statement) =>  reduce(condition, env) match {
        case Some(Bool(b))=>
        if (b) runStatement(While(condition, statement), runStatement(statement, env))
        else env
        case _ => throw CustomException("While loop condition reduced not to Bool type")
      }

      case Sequence(ls) => ls.foldLeft(env)((env, s) => runStatement(s, env))
    }
  }

  private def ifElseRun(cond: Expr, ifSt: Stat, elseSt: Stat, env: Map[String, Any]): Map[String, Any] = {
    if(cond.isReducible) ifElseRun(reductionStep(cond, env), ifSt, elseSt, env)
    else
      if (cond.toBool) runStatement(ifSt, env)
      else runStatement(elseSt, env)
  }

  private def assignRun(name: String, expr: Expr, env: Map[String, Any]):Map[String, Any] = {
    if(expr.isReducible) assignRun(name, reductionStep(expr, env), env)
    else {

      val value = expr match {
        case Bool(b) => b
        case Number(i) => i
        case _ => throw CustomException(s"Trying to assing not reduced expression $expr to variable $name")
      }

      printEnv(env + (name -> value))
      env + (name -> value)
    }
  }

}

