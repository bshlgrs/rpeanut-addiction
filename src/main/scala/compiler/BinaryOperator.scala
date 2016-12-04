package compiler

/**
  * Created by buck on 12/3/16.
  */
sealed abstract class BinaryOperator(symbol: String) {
  override def toString: String = symbol
}

object AddOperator extends BinaryOperator("+")
object MultiplyOperator extends BinaryOperator("*")
object SubtractOperator extends BinaryOperator("-")
object DivideOperator extends BinaryOperator("/")

object LessThanOperator extends BinaryOperator("<")
object GreaterThanOperator extends BinaryOperator(">")
object EqualsOperator extends BinaryOperator("==")
object NotEqualsOperator extends BinaryOperator("!=")