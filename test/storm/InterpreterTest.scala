package storm

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.scalatest._
import storm.parser.{StormLexer, StormParser}
import storm.runtime.Interpreter.ExecutionException
import storm.runtime.{Context, Interpreter, Value}


class InterpreterTest extends FunSuite {
  def eval(code: String): Value = {
    Interpreter.eval(new StormParser(new CommonTokenStream(new StormLexer(CharStreams.fromString(code)))).parse().result)(Context())
  }

  test("simple expressions") {
    assert(eval("") == Value.None)
    assert(eval("1") == Value(1))
    assert(eval("true") == Value(true))
    assert(eval("-7") == Value(-7))
    assert(eval("2 + 3 * 5") == Value(17))
    assert(eval("'a' + 'b'") == Value("ab"))
    assert(eval("'x = \\( 1 + 2 )...'") == Value("x = 3..."))
    assert(eval("math.abs(-7)") == Value(7))
    assert(eval("math.gcd(100, 170) / 5") == Value(2))
    assert(eval("if true then 1 else 2") == Value(1))
    assert(eval("(if 5 > 9 then {} else {x = 4, y = 've'}).y") == Value("ve"))
    assert(eval("1 < 5 == 2 + 3 < 2 + 6") == Value(true))
    assert(eval("1 < 5 < 3") == Value(false))
    assert(eval("f = (x, y) -> x + y - x * y; f(5, 7)") == Value(-23))
    assert(eval("let math = 1; math") == Value(1))
    assert(eval("f(x, y) = x * y - 2; f(3, 5)") == Value(13))
    assert(eval("x = 0; f() = x; x = 4; f()") == Value(4))
    assert(eval("let + = *; let / = -; 6 / 2 + 5") == Value(20))
    assert(eval("{x = 3 ^ 2 ^ 3, y = *()}") == Value.Record(Map("x" -> Value(6561), "y" -> Value(1))))
  }

  test("errors") {
    assertThrows[ExecutionException](eval("{x = 1, y = 2, x = 3}"))
    assertThrows[ExecutionException](eval("1 + 'b'"))
    assertThrows[ExecutionException](eval("x"))
    assertThrows[ExecutionException](eval("(3)(2, 5)"))
    assertThrows[scala.MatchError](eval("math.abs(true)"))
    assertThrows[ExecutionException](eval("math = 1"))
    assertThrows[ExecutionException](eval("(x = 1; 2); x"))
    assertThrows[ExecutionException](eval("f() = x; x = 4; f()"))
  }
}
