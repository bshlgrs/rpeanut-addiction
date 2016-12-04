package compiler

/**
  * Created by buck on 12/3/16.
  */
sealed abstract class CType {

}

case object Int extends CType
case object Bool extends CType
