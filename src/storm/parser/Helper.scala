package storm.parser

import scala.collection.JavaConverters._
import org.antlr.v4.runtime.{Token, TokenStream}
import storm.ast._
import storm.parser.StormParser.ExprContext


abstract class Helper(input: TokenStream) extends org.antlr.v4.runtime.Parser(input) {
  def seq(nodes: java.util.List[ExprContext]) = nodes.asScala.map(_.result)

  def bool(b: Boolean) = Bool(b)
  def integer(i: Token) = Integer(i.getText.toInt)
  def op(op: Token, expr: Node) = Unary(op.getText, expr)
  def op(left: Node, op: Token, right: Node) = Binary(left, op.getText, right)
  def if_(cond: Node, thenExpr: Node, elseExpr: Node) = If(cond, thenExpr, elseExpr)
}
