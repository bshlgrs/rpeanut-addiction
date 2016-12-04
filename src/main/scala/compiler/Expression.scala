package compiler

/**
  * Created by buck on 12/3/16.
  */
sealed abstract class Expression {
  override def toString: String = this match {
    case VariableExpression(name) => name
    case AssignmentExpression(name, exp) => s"($name = $exp)"
    case FunctionCallExpression(function, args) => s"$function(${args.map(_.toString).mkString(", ")}"
    case BinaryOperatorExpression(op, lhs, rhs) => s"($lhs $op $rhs)"
    case IntLiteralExpression(i) => i.toString
    case StringLiteralExpression(s) => "\"" ++ s.replaceAll("\"", "\\\"") ++ "\""
  }
}

case class VariableExpression(name: String) extends Expression
case class AssignmentExpression(name: String, exp: Expression) extends Expression
case class FunctionCallExpression(function: Expression, args: List[Expression]) extends Expression
case class BinaryOperatorExpression(op: BinaryOperator, lhs: Expression, rhs: Expression)  extends Expression
case class UnaryOperatorExpression(exp: Expression, op: UnaryOperator) extends Expression

case class IntLiteralExpression(int: Int) extends Expression
case class StringLiteralExpression(string: String) extends Expression

case class TernaryOperator(cond: Expression, lhs: Expression, rhs: Expression) extends Expression
