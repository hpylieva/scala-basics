sealed trait Expr {
  def isReducible: Boolean = this match {
    case Number(_) | Bool(_) => false
    case _ => true
  }

  def toInt: Int = this match {
    case Number(n) => n
    case _ => throw CustomException("Exception: Type "+ this.getClass.getName +
          " is not supported for the expression.")
  }

  def toBool: Boolean = this match {
    case Bool(b) => b
    case _ => throw CustomException("Exception: Type "+ this.getClass.getName +
      " is not supported for the expression.")
  }

  def +(that: Expr): Int = this.toInt + that.toInt
  def *(that: Expr): Int = this.toInt * that.toInt
  def <(that: Expr): Boolean = this.toInt < that.toInt

  def evaluate: Expr = {
    def evaluateOperand(Operand: Expr): Expr = Operand match{
      case Number(_) | Bool(_) => Operand
      case _ => Operand.evaluate
    }

    this match {
      case Number(n) => Number(n)
      case Bool(b) => Bool(b)
      case Sum(lOp, rOp) =>
        Number(evaluateOperand(lOp) + evaluateOperand(rOp))

      case Prod(lOp, rOp) =>
        Number(evaluateOperand(lOp) * evaluateOperand(rOp))

      case Less(lOp, rOp) =>
        Bool(evaluateOperand(lOp) < evaluateOperand(rOp))

      case IfElse(conditionExpr, ifExpr, elseExpr) =>
        if (conditionExpr.toBool) ifExpr.evaluate else elseExpr.evaluate
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

case class Empty() extends Expr