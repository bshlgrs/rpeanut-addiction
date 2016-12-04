package compiler

/**
  * Created by buck on 12/3/16.
  */
case class FunctionDefinition(name: String, cType: CType, args: List[(String, CType)], body: Option[List[Statement]]) {

}
