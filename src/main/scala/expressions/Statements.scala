sealed trait Statement{
    def doWork(expr: Expr): Expr = this match {
      case DoNothing => expr
    }

    override def toString: String = this match {
      case DoNothing => s""
      case Assign(varName, value) => s"$varName = $value"
      case IfElseStatement(condition, ifStatement, elseStatement) =>
        s"($condition) {\n$ifStatement\n} else {\n$elseStatement\n}"
      case Sequence(seq) => seq.map(s => s.toString).mkString("\n")
      case WhileLoop(condition, loop) => s"while($condition) do {\n$loop\n}"
    }
}

case object DoNothing extends Statement
case class Assign(varName: String, value: Expr) extends Statement
case class IfElseStatement(condition: Expr, ifStatement: Statement, elseStatement: Statement) extends Statement
case class Sequence(seq: List[Statement]) extends Statement
case class WhileLoop(condition: Expr, loop: Statement) extends Statement