package storm.parser

import scala.collection.JavaConverters._
import org.antlr.v4.runtime.{Token, TokenStream}
import storm.ast._
import storm.parser.StormParser.ExprContext


abstract class Helper(input: TokenStream) extends org.antlr.v4.runtime.Parser(input) {
  private def unescape(s: String): String = {
    // we also remove string delimiters; strings can start with ' " ) and end with ' " \(
    assert(Set('\'', '"', ')').contains(s.head))
    assert(Set('\'', '"').contains(s.last) || s.endsWith("\\("))
    val b = new StringBuilder(s.length - 2)
    var i = 1
    while(i < s.length - 1) {
      val c = s.charAt(i)
      if(c == '\\') {
        i = i + 1
        val e = s.charAt(i)
        if(!(e == '(' && s.length == i + 1)) {
          b.append(e match {
            case 't' => '\t'
            case 'n' => '\n'
            case 'r' => '\r'
            case '\'' => '\''
            case '"' => '"'
            case '\\' => '\\'
            case _ => throw new Exception(s"invalid escape: \\$e")
          })
        }
      } else {
        b.append(c)
      }
      i = i + 1
    }
    b.toString()
  }

  def seq(nodes: java.util.List[ExprContext]) = nodes.asScala.map(_.result)

  def bool(b: Boolean) = Bool(b)
  def integer(i: Token) = Integer(i.getText.toInt)
  def str(parts: java.util.List[Token], exprs: java.util.List[ExprContext]) = {
    if(parts.size() == 1) Str(unescape(parts.get(0).getText))
    else Interpolated(parts.asScala.map { part => unescape(part.getText) }, exprs.asScala.map(_.result))
  }
  def op(op: Token, expr: Node) = Unary(op.getText, expr)
  def op(left: Node, op: Token, right: Node) = Binary(left, op.getText, right)
  def if_(cond: Node, thenExpr: Node, elseExpr: Node) = If(cond, thenExpr, elseExpr)
}
