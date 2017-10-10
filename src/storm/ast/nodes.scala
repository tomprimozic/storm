package storm.ast


sealed abstract class Node

case class Ident(name: String) extends Node
case class Integer(i: Int) extends Node
case class Bool(b: Boolean) extends Node
case class Str(s: String) extends Node
case class Interpolated(parts: Seq[String], exprs: Seq[Node]) extends Node
case class Field(expr: Node, field: String) extends Node
case class Call(fn: Node, args: Seq[Node]) extends Node
case class Record(fields: Seq[(String, Node)]) extends Node
case class Arrow(params: Seq[String], body: Node) extends Node
case class Binary(left: Node, op: String, right: Node) extends Node
case class Unary(op: String, expr: Node) extends Node
case class Cmp(ops: Seq[String], exprs: Seq[Node]) extends Node
case class And(exprs: Seq[Node]) extends Node
case class Or(exprs: Seq[Node]) extends Node
case class If(cond: Node, thenExpr: Node, elseExpr: Node) extends Node
case class Assign(pattern: Node, expr: Node) extends Node
case class Declare(kind: Declare.Kind, pattern: Node, expr: Node) extends Node
case class Print(expr: Node) extends Node
case class Sequence(exprs: Seq[Node]) extends Node
case class While(cond: Node, body: Seq[Node]) extends Node
case class BlockIf(cond: Node, thenBlock: Seq[Node], elseBlock: Seq[Node]) extends Node
case class Return(expr: Option[Node]) extends Node
case object Continue extends Node
case object Break extends Node

object Declare {
  sealed abstract class Kind
  case object Var extends Kind
  case object Let extends Kind
}
