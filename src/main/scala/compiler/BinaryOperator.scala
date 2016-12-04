package compiler

/**
  * Created by buck on 12/3/16.
  */
sealed abstract class BinaryOperator(symbol: String) {

}

object AddOperator extends BinaryOperator("+")
object MultiplyOperator extends BinaryOperator("+")
object SubtractOperator extends BinaryOperator("+")
object DivideOperator extends BinaryOperator("+")

