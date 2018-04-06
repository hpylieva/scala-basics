case class CustomException(msg: String)  extends Exception


sealed trait Expr {
  def isReducible: Boolean = this match {
    case Number(_) => false
    case Bool(_) => false
    case _ => true
  }

  def processArgsOfBinaryOperation(Operand: Expr): Expr = Operand match{
    case Number(_) => Operand
    case _ => Operand.evaluate.asInstanceOf[Number]
  }

  def evaluate: Expr = this match {
    case Number(n) => Number(n)
    case Bool(b) => Bool(b)
    case Sum(lOp, rOp) => {
      Number(processArgsOfBinaryOperation(lOp).asInstanceOf[Number].n
        + processArgsOfBinaryOperation(rOp).asInstanceOf[Number].n)
    }

    case Prod(lOp, rOp) => {
      val left = lOp match {
        case Number(_) => lOp
        case _ => lOp.evaluate
      }
      val right = rOp match {
        case Number(_) => rOp
        case _ => rOp.evaluate
      }
      Number(processArgsOfBinaryOperation(lOp).asInstanceOf[Number].n
        * processArgsOfBinaryOperation(rOp).asInstanceOf[Number].n)
    }

    case Less(lOp, rOp) => {
      val left = lOp match {
        case Number(_) => lOp
        case _ => lOp.evaluate
      }
      val right = rOp match {
        case Number(_) => rOp
        case _ => rOp.evaluate
      }
      Bool(processArgsOfBinaryOperation(lOp).asInstanceOf[Number].n
        < processArgsOfBinaryOperation(rOp).asInstanceOf[Number].n)
    }

    case IfElse(conditionExpr, ifExpr, elseExpr) => {
      val c = conditionExpr match {
        case Bool(_) => conditionExpr
        case _ => conditionExpr.evaluate
      }

      if (c.asInstanceOf[Bool].b) ifExpr match{
        case Number(_) => ifExpr
        case Bool(_) => ifExpr
        case _ => ifExpr.evaluate
      } else elseExpr match {
        case Number(_) => elseExpr
        case Bool(_) => elseExpr
        case _ => elseExpr.evaluate
      }

    }
  }

  override def toString: String = this match {
    case Number(n) => n.toString
    case Bool(b) => b.toString
    case Var(name) => name
    case Sum(lOp, rOp) => s"$lOp + $rOp"
    case Prod(lOp, rOp) => {
      def setParentheses(Op: Expr): String = Op match {
        case Sum(_, _) => s"($Op)"
        case _ => s"$Op"
      }
      setParentheses(lOp) + "*" + setParentheses(rOp)
    }

    case Less(lOp, rOp) => {
      val left = if (lOp.isReducible) s"($lOp)" else s"$lOp"
      val right = if (rOp.isReducible) s"($rOp)" else s"$rOp"

      s"$left < $right"
    }

    case IfElse(conditionExpr, ifExpr, elseExpr) => s"if ($conditionExpr) then $ifExpr else $elseExpr"
  }

}

case class Number(n: Int) extends Expr

case class Bool(b: Boolean) extends Expr

case class Var(name: String) extends Expr

case class Sum(lOp: Expr, rOp: Expr) extends Expr

case class Prod(lOp: Expr, rOp: Expr) extends Expr

case class Less(lOp: Expr, rOp: Expr) extends Expr

case class IfElse(conditionExpr: Expr, ifExpr: Expr, elseExpr: Expr) extends Expr

//case class IfElse()