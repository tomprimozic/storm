package storm.ast


sealed abstract class Node

case class Integer(i: Int) extends Node
case class Bool(b: Boolean) extends Node
case class Binary(left: Node, op: String, right: Node) extends Node
case class Unary(op: String, expr: Node) extends Node
case class If(cond: Node, thenExpr: Node, elseExpr: Node) extends Node