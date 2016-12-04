package compiler

/**
  * Created by buck on 12/3/16.
  */
sealed abstract class UnaryOperator(symbol: String) {

}

object PointerDereferenceOperator extends UnaryOperator("*")
