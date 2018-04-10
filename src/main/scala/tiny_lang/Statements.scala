sealed trait Statement {
  def isReducible: Boolean = this match {
    case DoNothing => false
    case _ => true
  }

  override def toString: String = this match {
    case DoNothing => s"-end-"
    case Assign(varName, value) => s"$varName = $value"
    case IfElseStatement(condition, ifStatement, elseStatement) =>
      s"($condition) {\n$ifStatement\n} else {\n$elseStatement\n}"
    case WhileLoop(condition, loopSt) => s"while ($condition) do {\n$loopSt\n}"
    case Sequence(list) => s"Sequence:\n{" + list.map(s => s.toString).mkString(",\n") + "}"
  }
}

case object DoNothing extends Statement
case class Assign(varName: String, value: Expr) extends Statement
case class IfElseStatement(condition: Expr, ifStatement: Statement, elseStatement: Statement) extends Statement
case class WhileLoop(condition: Expr, loopSt: Statement) extends Statement
case class Sequence(list: List[Statement]) extends Statement
