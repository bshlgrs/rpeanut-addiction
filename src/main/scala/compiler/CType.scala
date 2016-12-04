package compiler

/**
  * Created by buck on 12/3/16.
  */
sealed abstract class CType {

}

case object CIntType extends CType
case object CBoolType extends CType

object CType {
  val nameMapping: Map[String, CType] = Map("int" -> CIntType, "bool" -> CBoolType)
}