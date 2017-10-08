package storm

import org.antlr.v4.runtime._
import org.scalatest.FunSuite
import storm.ast._
import storm.parser.{StormLexer, StormParser}

import scala.language.implicitConversions


class ParserTest extends FunSuite {
  implicit def Boolean2Node(b: Boolean): Node = Bool(b)
  implicit def Int2Node(i: Int): Node = Integer(i)

  implicit val testErrorListener: ANTLRErrorListener = ConsoleErrorListener.INSTANCE

  def parse(code: String)(implicit errorListener: ANTLRErrorListener): Seq[Node] = {
    val lexer = new StormLexer(CharStreams.fromString(code))
    lexer.removeErrorListeners()
    lexer.addErrorListener(errorListener)
    val parser = new StormParser(new CommonTokenStream(lexer))
    parser.removeErrorListeners()
    parser.addErrorListener(errorListener)
    parser.parse().result
  }

  def nodes(n: Node*): Seq[Node] = n

  test("expressions") {
    assert(parse("") == Seq.empty[Node])
    assert(parse("'abc'") == nodes(Str("abc")))
    assert(parse("-9") == nodes(Unary("-", 9)))
    assert(parse("false") == nodes(false))
    assert(parse("2 + 7") == nodes(Binary(2, "+", 7)))
    assert(parse("if true then -1 else (2 + 4 * 9)") == nodes(If(true, Unary("-", 1), Binary(2, "+", Binary(4, "*", 9)))))
    assert(parse("\"x\"") == nodes(Str("x")))
    assert(parse("{x = 1, y = 2, x = 3}") == nodes(Record(Seq(("x", 1), ("y", 2), ("x", 3)))))
  }

  test("string interpolation") {
    assert(parse("'abc\\( 'pqr' )xyz'") == nodes(Interpolated(Seq("abc", "xyz"), Seq(Str("pqr")))))
    assert(parse("\"11\\( (2 + 3) * 4 + \"22\\( true )33\" )44\\( 7 - 1 )55\"") == nodes(Interpolated(Seq("11", "44", "55"), Seq(Binary(Binary(Binary(2, "+", 3), "*", 4), "+", Interpolated(Seq("22", "33"), Seq(true))), Binary(7, "-", 1)))))
  }

  test("errors") {
    class Exception(msg: String, line: Int, column: Int, cause: RecognitionException) extends java.lang.Exception(msg, cause) {
      override def toString = s"lexer error: line $line:$column $msg${ if(cause != null) cause.toString else "" }"
    }

    implicit val errorListener: BaseErrorListener = new BaseErrorListener {
      override def syntaxError(recognizer: Recognizer[_, _], offendingSymbol: Any, line: Int, charPositionInLine: Int, msg: String, e: RecognitionException): Unit = {
        throw new Exception(msg, line, charPositionInLine, e)
      }
    }

    assertThrows[Exception] { parse("'xxx") }
    assertThrows[Exception] { parse("'\\c") }
  }

}
