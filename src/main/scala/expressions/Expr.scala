case class CustomException(msg: String)  extends Exception


sealed trait Expr {
  def isReducible: Boolean = this match {
    case Number(_) => false
    case Bool(_) => false
    case _ => true
  }

  def evaluate: Expr = this match {
    case Number(n) => Number(n)
    case Bool(b) => Bool(b)
    case Sum(lOp, rOp) => {
      val left = lOp match {
        case Number(_) => lOp
        case _ => lOp.evaluate
      }
      val right = rOp match {
        case Number(_) => rOp
        case _ => rOp.evaluate
      }
      Number(left.asInstanceOf[Number].n + right.asInstanceOf[Number].n)
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
      Number(left.asInstanceOf[Number].n * right.asInstanceOf[Number].n)
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
      Bool(left.asInstanceOf[Number].n < right.asInstanceOf[Number].n)
    }
  }

  override def toString: String = this match {
    case Number(n) => n.toString
    case Bool(b) => b.toString
    case Var(name) => name
    case Sum(lOp, rOp) => s"$lOp + $rOp"
    case Prod(lOp, rOp) => lOp match {
      case Sum(_, _) => rOp match {
        case Sum(_, _) => s"($lOp) * ($rOp)"
        case _ => s"($lOp) * $rOp"
      }
      case _ => rOp match {
        case Sum(_, _) => s"$lOp * ($rOp)"
        case _ => s"$lOp * $rOp"
      }
    }
    case Less(lOp, rOp) => s"$lOp < $rOp"
  }
}

case class Number(n: Int) extends Expr

case class Bool(b: Boolean) extends Expr

case class Var(name: String) extends Expr

case class Sum(lOp: Expr, rOp: Expr) extends Expr

case class Prod(lOp: Expr, rOp: Expr) extends Expr

case class Less(lOp: Expr, rOp: Expr) extends Expr

//case class IfElse()