case class CustomException(msg: String)  extends Exception

sealed trait Expr {
  def isReducible: Boolean
  def eval: Int
}

//  def eval: Int = this match {
//    case Number(n) => n
//    case Sum(lOp, rOp) => lOp.eval + rOp.eval
//    case Prod(lOp, rOp) => lOp.eval * rOp.eval
//    case Var(name) => 0
//  }

case class Number(n: Int) extends Expr{
  override def isReducible: Boolean = false
  override def toString: String = n.toString
  override def eval: Int = n
}

case class Bool(b: Boolean) extends Expr{
  override def isReducible: Boolean = false
  override def toString: String = b.toString
  override def eval: Int = 0
}

case class Var(name: String) extends Expr{
  override def isReducible: Boolean = true
  override def toString: String = name
  override def eval: Int = 0 // TODO:
}

case class Sum(lOp: Expr, rOp: Expr) extends Expr{
  override def isReducible: Boolean = true
  override def toString: String = s"$lOp + $rOp"
  override def eval: Int = lOp.eval + rOp.eval
}

case class Prod(lOp: Expr, rOp: Expr) extends Expr{
  override def isReducible: Boolean = true
  override def toString: String = lOp match {
    case Sum(_, _) => rOp match {
      case Sum(_, _) => s"($lOp) * ($rOp)"
      case _ => s"($lOp) * $rOp"
    }
    case _ => rOp match {
      case Sum(_, _) => s"$lOp * ($rOp)"
      case _ => s"$lOp * $rOp"
    }
  }
  override def eval: Int = lOp.eval * rOp.eval
}

case class Less(lOp: Expr, rOp: Expr) extends Expr{
  override def isReducible: Boolean = true
  override def toString: String = s"$lOp < $rOp"

  override def eval: Int = 0 //todo
}
