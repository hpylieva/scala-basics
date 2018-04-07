
final class Machine(environment:  Map[String, Expr]){
  var env: Map[String, Expr] = environment
  var cond: Expr = Empty()

  def run(expr: Expr): Option[Expr] = {
    println(expr)

    if (expr.isReducible) {
      try {
        run(reductionStep(expr))
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

  def reductionStep(expr: Expr): Expr = {

    def reduceBinaryOperation(applyFunc: (Expr, Expr) => Expr, lOp: Expr, rOp: Expr): Expr = {
      if (lOp.isReducible) applyFunc(reductionStep(lOp), rOp)
      else if (rOp.isReducible) applyFunc(lOp, reductionStep(rOp))
      else expr.evaluate
    }

    expr match {
      case Number(_) | Bool(_) => expr
      case Prod(lOp, rOp) => reduceBinaryOperation(Prod.apply, lOp, rOp)
      case Sum(lOp, rOp) => reduceBinaryOperation(Sum.apply, lOp, rOp)
      case Var(name) => {
        if (env contains name) env(name)
        else throw CustomException("Exception: Var name is not present in Environment.")
      }

      case Less(lOp, rOp) => reduceBinaryOperation(Less.apply, lOp, rOp)

      case IfElse(conditionExpr, ifExpr, elseExpr) => {
        if (conditionExpr.isReducible) IfElse(reductionStep(conditionExpr), ifExpr, elseExpr)
        else
          if (conditionExpr.toBool) reductionStep(ifExpr)
            else reductionStep(elseExpr)
      }
    }
  }

  def run(statement: Statement): Option[Statement] = {
    println(s"Environment: $env\nRunning statement:\n$statement\n")
    if (statement.isReducible) {
      try {
        run(reductionStep(statement))
      } catch {
        case exception: CustomException => println(exception.msg)
          None
      }
    }
    else
    Option(statement)
  }

  def printEnv(): Unit =  println(s"Environment:\n$env")

  private def reductionStep(statement: Statement): Statement = statement match{

    case Assign(name, expr) => {
      if (expr.isReducible) Assign(name, reductionStep(expr) )
      else {
        env+= name -> expr
        DoNothing
      }
    }
    case IfElseStatement(condition,ifSt,elseSt) => {
      if (condition.isReducible)
        IfElseStatement(reductionStep(condition), ifSt, elseSt)
      else
        if (condition.toBool) reductionStep(ifSt)
          else reductionStep(elseSt)
    }

    case WhileLoop(condition, loopSt) => {
      if (cond.equals(Empty())) cond = condition

      if (condition.isReducible)
        WhileLoop(reductionStep(condition), loopSt)
      else
        if (condition.toBool) {
          this.run(loopSt)
          WhileLoop(cond,loopSt)
        }
        else {
        cond = Empty()
        DoNothing
        }
    }

    case Sequence(list) => {
      if (list.nonEmpty){
        this.run(list.head)
        if (list.length > 1)
          Sequence(list.drop(1))
        else DoNothing
      }
      else DoNothing
    }


  }


}

