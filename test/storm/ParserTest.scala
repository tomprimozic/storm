package storm

import org.antlr.v4.runtime._
import org.scalatest.FunSuite
import storm.ast._
import storm.parser.{StormLexer, StormParser}

import scala.language.implicitConversions


class ParserTest extends FunSuite {
  implicit def Boolean2Node(b: Boolean): Node = Bool(b)
  implicit def Int2Node(i: Int): Node = Integer(i)
  implicit def String2Ident(s: String): Node = Ident(s)

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
    assert(parse("x_1 + test-case") == nodes(Binary(Ident("x_1"), "+", Ident("test-case"))))
    assert(parse("-9") == nodes(Unary("-", 9)))
    assert(parse("false") == nodes(false))
    assert(parse("2 + 7") == nodes(Binary(2, "+", 7)))
    assert(parse("if true then -1 else (2 + 4 * 9)") == nodes(If(true, Unary("-", 1), Binary(2, "+", Binary(4, "*", 9)))))
    assert(parse("\"x\"") == nodes(Str("x")))
    assert(parse("{x = 1, y = 2, x = 3}") == nodes(Record(Seq(("x", 1), ("y", 2), ("x", 3)))))
    assert(parse("x -> if y then x else x * 2") == nodes(Arrow(Seq("x"), If("y", "x", Binary("x", "*", 2)))))
    assert(parse("f(2, (x = 4; x), 7)") == nodes(Call("f", Seq(2, Sequence(Seq(Assign("x", 4), "x")), 7))))
    assert(parse("[[], {}][4, 5,]") == nodes(Item(List(Seq(List(Seq.empty), Record(Seq.empty))), Seq(4, 5))))
    assert(parse("{useless = () -> {}, add = (x, y) -> x + y}") == nodes(Record(Seq(("useless", Arrow(Seq.empty, Record(Seq.empty))), ("add", Arrow(Seq("x", "y"), Binary("x", "+", "y")))))))
    assert(parse("+") == nodes(Ident("+")))
    assert(parse("-(8)") == nodes(Unary("-", 8)))
    assert(parse("-(8, 5)") == nodes(Call("-", Seq(8, 5))))
    assert(parse("-2^4^6") == nodes(Unary("-", Binary(2, "^", Binary(4, "^", 6)))))
    assert(parse("-(2)^4") == nodes(Unary("-", Binary(2, "^", 4))))
    assert(parse("*(2)^4") == nodes(Binary(Call("*", Seq(2)), "^", 4)))
    assert(parse("-(2, 6)^3") == nodes(Binary(Call("-", Seq(2, 6)), "^", 3)))
    assert(parse("exit((while true {}))") == nodes(Call("exit", Seq(While(Bool(true), Seq.empty)))))
    assert(parse("^ + * > < < ^ < / / /") == nodes(Cmp(Seq(">", "<", "<"), Seq(Binary("^", "+", "*"), "<", "^", Binary("/", "/", "/")))))
    assert(parse("(x, y, ) -> {x = [f(1, ), ], }") == nodes(Arrow(Seq("x", "y"), Record(Seq(("x", List(Seq(Call("f", Seq(1))))))))))
    assert(parse("x++1") == nodes(Binary("x", "+", Unary("+", 1))))
  }

  test("string interpolation") {
    assert(parse("'abc\\( 'pqr' )xyz'") == nodes(Interpolated(Seq("abc", "xyz"), Seq(Str("pqr")))))
    assert(parse("\"11\\( (2 + 3) * 4 + \"22\\( true )33\" )44\\( 7 - 1 )55\"") == nodes(Interpolated(Seq("11", "44", "55"), Seq(Binary(Binary(Binary(2, "+", 3), "*", 4), "+", Interpolated(Seq("22", "33"), Seq(true))), Binary(7, "-", 1)))))
  }

  test("statements") {
    assert(parse("print 1") == nodes(Print(1)))
    assert(parse("x = 6; x") == nodes(Assign("x", 6), "x"))
    assert(parse("let x = 1; var y = 2; f(x) = x + 1") == nodes(Declare(Declare.Let, "x", 1), Declare(Declare.Var, "y", 2), Assign(Call("f", Seq("x")), Binary("x", "+", 1))))
    assert(parse("sum(list) = list.fold(+)") == nodes(Assign(Call("sum", Seq("list")), Call(Field("list", "fold"), Seq("+")))))
    assert(parse("while x < 5 { x -= 1; print x }") == nodes(While(Cmp(Seq("<"), Seq("x", 5)), Seq(Binary("x", "-=", 1), Print("x")))))
    assert(parse("if x { 1 } else { if y { 2 } }") == nodes(BlockIf("x", Seq(1), Seq(BlockIf("y", Seq(2), Seq.empty)))))
    assert(parse("if x { break } else if y { continue } else if z { return }") == nodes(BlockIf("x", Seq(Break), Seq(BlockIf("y", Seq(Continue), Seq(BlockIf("z", Seq(Return(None)), Seq.empty)))))))
    assert(parse("return 4") == nodes(Return(Some(4))))
    assert(parse("function f(x) { return x + 1 }") == nodes(Function(Call("f", Seq("x")), Seq(Return(Some(Binary("x", "+", 1)))))))
    assert(parse("fun x * y { y - x }") == nodes(Function(Binary("x", "*", "y"), Seq(Binary("y", "-", "x")))))
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
    assertThrows[Exception] { parse("{,}") }
    assertThrows[Exception] { parse("+(4, 2).field") }
    assertThrows[Exception] { parse("[,]") }
    assertThrows[Exception] { parse("(,)") }
    assertThrows[Exception] { parse("()") }
    assertThrows[Exception] { parse("exit(while true {})") }
  }
}
