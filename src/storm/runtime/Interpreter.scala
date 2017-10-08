package storm.runtime

import storm.ast._

import scala.annotation.tailrec


case class Context(bindings: Map[String, Value])

object Context {
  val builtins = Context(Map(
    "==" -> Builtins.`==`,
    "!=" -> Builtins.`!=`,
    "<=" -> Builtins.`<=`,
    ">=" -> Builtins.`>=`,
    "<" -> Builtins.`<`,
    ">" -> Builtins.`>`,
    "+" -> Builtins.`+`,
    "-" -> Builtins.`-`,
    "*" -> Builtins.`*`,
    "/" -> Builtins.`/`,
    "math" -> Value.Record(Map(
      "abs" -> Builtins.`math.abs`,
      "gcd" -> Builtins.`math.gcd`
    ))
  ))
}

object Interpreter {
  case class ExecutionException(message: String) extends Exception(message)
  def error(message: String) = throw ExecutionException(message)

  def eval(nodes: Seq[Node])(implicit context: Context): Value = nodes.foldLeft[Value](Value.None) { case (_, node) => eval(node) }

  def eval(node: Node)(implicit context: Context): Value = node match {
    case Ident(name) => context.bindings.getOrElse(name, error(s"unknown variable $name"))
    case Integer(i) => Value(i)
    case Bool(b) => Value(b)
    case Str(s) => Value(s)
    case Interpolated(parts, exprs) =>
      val partsIter = parts.iterator
      val exprsIter = exprs.iterator
      val result = new StringBuilder(partsIter.next())
      while(exprsIter.hasNext) {
        result.append(Value.toString(eval(exprsIter.next())))
        result.append(partsIter.next())
      }
      assert(!partsIter.hasNext)
      Value(result.toString())
    case Field(expr, field) => eval(expr).record.fields(field)
    case Call(fn, exprs) => eval(fn).function.apply(exprs.map(eval))
    case Record(fields) =>
      val record = scala.collection.mutable.Map[String, Value]()
      fields.foreach { case (field, expr) =>
        if(record.contains(field)) error(s"duplicate field $field")
        record += field -> eval(expr)
      }
      Value.Record(record.toMap)
    case Binary(left, op, right) => eval(Call(Ident(op), Seq(left, right)))
    case Unary(op, expr) => eval(Call(Ident(op), Seq(expr)))
    case Cmp(ops, exprs) =>
      assert(ops.nonEmpty && exprs.size == ops.size + 1)
      @tailrec
      def loop(previous: Value, ops: Seq[String], exprs: Seq[Node]): Value = {
        if(ops.isEmpty) Value.True else {
          val next = eval(exprs.head)
          val op = context.bindings(ops.head).function
          val result = op.apply(Seq(previous, next)).bool.value
          if(result) loop(next, ops.tail, exprs.tail) else Value.False
        }
      }
      loop(eval(exprs.head), ops, exprs.tail)
    case And(exprs) =>
      assert(exprs.nonEmpty)
      Value(exprs.forall(eval(_).bool.value))
    case Or(exprs) =>
      assert(exprs.nonEmpty)
      Value(exprs.exists(eval(_).bool.value))
    case If(cond, thenExpr, elseExpr) => if(eval(cond).bool.value) eval(thenExpr) else eval(elseExpr)
  }
}
