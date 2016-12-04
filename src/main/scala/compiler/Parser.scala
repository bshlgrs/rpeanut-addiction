package compiler

import fastparse.WhitespaceApi

/**
  * Created by buck on 12/3/16.
  */
object Parser {
  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import fastparse.noApi._
  import White._

  lazy val name: P[String] = P(CharIn('a' to 'z', 'A' to 'Z', Seq('_')).repX(1).!)
  val nameExpr: P[Expression] = name.map(VariableExpression)
  val number: P[Expression] = P( CharIn('0'to'9').rep(1).!.map(VariableExpression))

  lazy val parens: P[Expression] = P( "(" ~/ addSub ~ ")" )
  lazy val factor: P[Expression] = P( number | assignmentExpression | nameExpr | parens )

  lazy val assignmentExpression: P[Expression] = P(name ~ "=" ~ addSub).map({ case (name: String, expr: Expression) =>
    AssignmentExpression(name, expr)
  })

  val divMul: P[Expression] = P((factor ~ (CharIn("*/").! ~/ factor).rep).map({ case ((e: Expression, s: Seq[(String, Expression)])) =>
    s.foldLeft(e)({ case (accExpr, (op, newE)) =>
      BinaryOperatorExpression(if (op == "*") MultiplyOperator else DivideOperator, accExpr, newE)
    })
  }))

  val addSub: P[Expression] = P((divMul ~ (CharIn("+-").! ~/ divMul).rep ).map({ case ((e: Expression, s: Seq[(String, Expression)])) =>
    s.foldLeft(e)({ case (accExpr, (op, newE)) =>
      BinaryOperatorExpression(if (op == "+") AddOperator else SubtractOperator, accExpr, newE)
    })
  }))

  val nakedExpr: P[Expression] = P( addSub ~ End )

  lazy val statementParser: P[Statement] = P(declStatement | returnStatementParser | ifStatementParser | exprStatementParser)

  lazy val declStatement: P[Statement] = P(ctype ~ name ~ ("=" ~ addSub).? ~ ";").map({ case (ctype: CType, name: String, init: Option[Expression]) =>
    DeclarationStatement(name, ctype, init)})

  lazy val exprStatementParser: P[Statement] = P((addSub ~ ";").map(ExpressionStatement))
  lazy val returnStatementParser: P[Statement] = P("return" ~ addSub ~ ";").map(ReturnStatement)
  lazy val ifStatementParser: P[Statement] = {
    P("if (" ~ addSub ~ ")" ~ "{" ~ statementParser.rep ~ "}" ~ ("else" ~ "{" ~ statementParser.rep ~ "}").?)
      .map({ case (cond: Expression, body: Seq[Statement], elseCase: Option[Seq[Statement]]) =>
          IfStatement(cond, body.toList, elseCase.map(_.toList).getOrElse(Nil))
      })
  }

  val nakedStmt = P(statementParser ~ End)

  val ctype: P[CType] = P("int".! | "bool".!).map((str: String) => CType.nameMapping(str))

  def main(args: Array[String]): Unit = {
    println(nakedStmt.parse("int x = 2 * 3;").get.value)
  }
}
