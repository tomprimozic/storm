package storm.runtime

import scala.language.implicitConversions


object Builtins {
  val `+`: Value.Function = { values =>
    if(values.forall(_.isInstanceOf[Value.Integer])) Value(values.foldLeft(0) { case (s, x) => s + x.int.value })
    else if(values.forall(_.isInstanceOf[Value.Str])) {
      val b = new StringBuilder
      values.foreach { case Value.Str(s) => b.append(s) }
      Value.Str(b.toString())
    } else {
      Interpreter.error("invalid arguments for `+`")
    }
  }

  val `-`: Value.Function = {
    case Seq(Value.Integer(x)) => Value.Integer(-x)
    case Seq(Value.Integer(x), Value.Integer(y)) => Value.Integer(x - y)
  }

  val `*`: Value.Function = { values => Value(values.foldLeft(1) { case (x, y) => x * y.int.value }) }

  val `==`: Value.Function = { case Seq(left, right) => Value(left == right) }
  val `!=`: Value.Function = { case Seq(left, right) => Value(left != right) }

  private def cmp(cmpI: (Int, Int) => Boolean, cmpS: (String, String) => Boolean): Value.Function = {
    case Seq(Value.Integer(x), Value.Integer(y)) => Value(cmpI(x, y))
    case Seq(Value.Str(x), Value.Str(y)) => Value(cmpS(x, y))
  }

  val `<` = cmp(_ < _, _ < _)
  val `>` = cmp(_ > _, _ > _)
  val `<=` = cmp(_ <= _, _ <= _)
  val `>=` = cmp(_ >= _, _ >= _)

  private def fn_I_I(fn: Int => Int): Value.Function = { case Seq(Value.Integer(i)) => Value(fn(i)) }
  private def fn_II_I(fn: (Int, Int) => Int): Value.Function = { case Seq(Value.Integer(x), Value.Integer(y)) => Value(fn(x, y)) }

  val `/` = fn_II_I(_ / _)
  val `^` = fn_II_I(Math.pow(_, _).toInt)
  val `math.abs` = fn_I_I(_.abs)
  val `math.gcd` = fn_II_I((x, y) => BigInt(x).gcd(BigInt(y)).toInt)
  val `math.min` = fn_II_I(_.min(_))
  val `math.max` = fn_II_I(_.max(_))
  val `math.mod` = fn_II_I(_ % _)
}
