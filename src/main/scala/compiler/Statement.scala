package compiler

/**
  * Created by buck on 12/3/16.
  */
sealed abstract class Statement {
  def niceString(level: Int = 0): String = {
    val indents = "  " * level
    this match {
      case DeclarationStatement(name, ctype, None) => s"$indents$ctype $name;"
      case DeclarationStatement(name, ctype, Some(value)) => s"$indents$ctype $name = $value;"
      case ExpressionStatement(exp) => indents + exp.toString + ";"
      case WhileStatement(cond, body) => indents + s"while ($cond) {" ++ body.map(_.niceString(level + 1)).mkString("\n") ++ indents ++ "}"
      case IfStatement(cond, body, Nil) => indents + s"if ($cond) {" ++ body.map(_.niceString(level + 1)).mkString("\n") ++ indents ++ "}"
      case IfStatement(cond, body, elseCase) => {
        indents + s"if ($cond) {" ++
            body.map(_.niceString(level + 1)).mkString("\n") ++
          indents ++ "} else {" ++
            elseCase.map(_.niceString(level + 1)).mkString("\n") ++
          indents ++ "}"
      }
      case ReturnStatement(exp) => s"return $exp;"
    }
  }
}

case class DeclarationStatement(name: String, ctype: CType, value: Option[Expression]) extends Statement
case class ExpressionStatement(exp: Expression) extends Statement
case class WhileStatement(cond: Expression, body: List[Statement]) extends Statement
case class IfStatement(cond: Expression, body: List[Statement], elseCase: List[Statement]) extends Statement
case class ReturnStatement(exp: Expression) extends Statement

