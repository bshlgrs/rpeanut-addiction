package compiler

/**
  * Created by buck on 12/3/16.
  */
sealed abstract class Expression {

}

case class VariableExpression(name: String) extends Expression
case class FunctionCallExpression(function: Expression, args: List[Expression]) extends Expression
case class BinaryOperatorExpression(lhs: Expression, rhs: Expression, op: BinaryOperator)  extends Expression
case class UnaryOperatorExpression(exp: Expression, op: UnaryOperator) extends Expression

case class IntLiteralExpression(int: Int) extends Expression
case class StringLiteralExpression(string: String) extends Expression

case class TernaryOperator(cond: Expression, lhs: Expression, rhs: Expression) extends Expression
