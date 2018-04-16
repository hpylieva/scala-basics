sealed trait Stat{

    override def toString: String = this match {
      case DoNothing => "-end-"
      case Assign(varName, value) => s"$varName = $value"
      case IfElseStat(condition, ifStatement, elseStatement) =>
        s"($condition) {\n$ifStatement\n} else {\n$elseStatement\n}"
      case While(condition, loopSt) => s"while ($condition) do {\n$loopSt\n}"
      case Sequence(list) => s"Sequence:\n{"+list.map(s => s.toString).mkString(",\n")  +"}"
    }
}

case object DoNothing extends Stat
case class Assign(varName: String, value: Expr) extends Stat
case class IfElseStat(condition: Expr, ifStatement: Stat, elseStatement: Stat) extends Stat
case class While(condition: Expr, loopSt: Stat) extends Stat
case class Sequence(list: List[Stat]) extends Stat
