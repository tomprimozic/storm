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
    assert(eval("{x = 3 ^ 2 ^ 3, y = *()}") == Value.Record(collection.mutable.Map("x" -> Value(6561), "y" -> Value(1))))
    assert(eval("x = 0; while x < 10 { x = x + 1; break }; x") == Value(1))
    assert(eval("x = 0; y = 0; while x < 10 { x = x + 1; if math.mod(x, 3) == 0 { continue } else { y = y + 1 } }; y") == Value(7))
    assert(eval("f = () -> (return 2; 3); f()") == Value(2))
    assert(eval("a = []; a.append(4); a.append(2); a.length") == Value(2))
    assert(eval("fib(n) = if n < 2 then n else fib(n - 2) + fib(n - 1); fib(7)") == Value(13))
    assert(eval("[...[-1, 0], ...(1..5), 5] == [-1, 0, 1, 2, 3, 4, 5]") == Value(true))
    assert(eval("x = [4, 5, 6]; x[1] = 7; x") == Value.List(Value(4), Value(7), Value(6)))
    assert(eval("h = {}; h.v = true; h.v") == Value(true))
    assert(eval("last(l) = l[-1]; last([5, 2, 8])") == Value(8))
    assert(eval("let < = >; let == = !=; 4 < 2 == 6") == Value(true))
    assert(eval("x = [...(1...6)]; for y in 0..3 { x.append(x.pop(0)) }; x") == Value.List(Value(4), Value(5), Value(6), Value(1), Value(2), Value(3)))
  }

  test("execution order") {
    assert(eval("x = 0; (x += 2; math.max)((x += 7; 2), (x *= 3; 5)); x") == Value(27))
    assert(eval("x = {a = 7}; y = 0; (y += 2; x).a += (y *= 3; 5); [x.a, y]") == Value.List(Value(12), Value(6)))
    assert(eval("x = [-1]; y = 1; (y += 3; x)[0] *= (y *= 7; 3); [x[0], y]") == Value.List(Value(-3), Value(28)))
    assert(eval("x = 0; (x *= 2; {}).a = (x += 9; 5); x") == Value(18))
    assert(eval("z = 3; (z *= 7; [true, false])[1] = (z += 2; 'b'); z") == Value(35))
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
    assertThrows[ExecutionException](eval("return 1"))
    assertThrows[ExecutionException](eval("continue"))
    assertThrows[ExecutionException](eval("break"))
    assertThrows[ExecutionException](eval("while true { (() -> (break))() }"))
  }
}
