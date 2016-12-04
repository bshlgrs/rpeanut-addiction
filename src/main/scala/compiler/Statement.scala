package compiler

/**
  * Created by buck on 12/3/16.
  */
sealed abstract class Statement {

}

case class DeclarationStatement(name: String, ctype: CType, value: Option[Expression])
case class ExpressionStatement(exp: Expression) extends Statement
case class WhileStatement(cond: Expression, body: List[Statement]) extends Statement
case class IfStatement(cond: Expression, body: List[Statement], elseCase: List[Statement]) extends Statement
case class ReturnStatement(exp: Expression) extends Statement

