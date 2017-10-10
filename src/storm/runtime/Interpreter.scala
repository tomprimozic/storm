package storm.runtime

import storm.ast._

import scala.annotation.tailrec


class Binding(val mutable: Boolean, var value: Value)
class Context private (parent: Option[Context] = None, val inLoop: Boolean = false, val inFunction: Boolean = false) {
  val bindings = collection.mutable.Map[String, Binding]()
  def child(inLoop: Boolean = inLoop, inFunction: Boolean = inFunction) = new Context(parent = Some(this), inLoop = inLoop, inFunction = inFunction)
  def copy() = {
    val result = new Context(parent)
    result.bindings ++= bindings
    result
  }
  def lookup(name: String): Option[Binding] = bindings.get(name).orElse(parent.flatMap(_.lookup(name)))
  def value(name: String) = lookup(name).getOrElse(Interpreter.error(s"$name is not defined")).value
}

object Context {
  private val builtins = {
    val context = new Context()
    Map(
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
      "^" -> Builtins.`^`,
      "math" -> Value.Record(Map(
        "abs" -> Builtins.`math.abs`,
        "gcd" -> Builtins.`math.gcd`,
        "max" -> Builtins.`math.max`,
        "min" -> Builtins.`math.min`,
        "mod" -> Builtins.`math.mod`
      ))
    ).foreach { case (name, value) => context.bindings += name -> new Binding(mutable = false, value) }
    context
  }

  def apply() = builtins.child()
}

object Interpreter {
  case class ExecutionException(message: String) extends Exception(message)
  def error(message: String) = throw ExecutionException(message)
  def `not implemented` = error("not implemented")

  case class ReturnException(value: Option[Value]) extends RuntimeException
  case object BreakException extends RuntimeException
  case object ContinueException extends RuntimeException

  def eval(nodes: Seq[Node])(implicit context: Context): Value = nodes.foldLeft[Value](Value.None) { case (_, node) => eval(node) }

  def eval(node: Node)(implicit context: Context): Value = node match {
    case Ident(name) => context.value(name)
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
      val record = collection.mutable.Map[String, Value]()
      fields.foreach { case (field, expr) =>
        if(record.contains(field)) error(s"duplicate field $field")
        record += field -> eval(expr)
      }
      Value.Record(record.toMap)
    case Arrow(params, body) => new Value.Function {
      val closure = context.copy()
      def apply(values: Seq[Value]) = {
        if (values.size != params.size) error("unexpected number of parameters")
        val context = closure.child(inFunction = true, inLoop = false)
        params.zip(values).foreach { case (parameter, value) => context.bindings += parameter -> new Binding(mutable = false, value) }
        try {
          eval(body)(context)
        } catch {
          case ReturnException(value) => value.getOrElse(Value.None)
        }
      }
    }
    case Binary(left, op, right) => eval(Call(Ident(op), Seq(left, right)))
    case Unary(op, expr) => eval(Call(Ident(op), Seq(expr)))
    case Cmp(ops, exprs) =>
      assert(ops.nonEmpty && exprs.size == ops.size + 1)
      @tailrec
      def loop(previous: Value, ops: Seq[String], exprs: Seq[Node]): Value = {
        if(ops.isEmpty) Value.True else {
          val next = eval(exprs.head)
          val op = context.value(ops.head).function
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
    case Assign(Ident(name), expr) =>
      context.lookup(name) match {
        case None => context.bindings += name -> new Binding(mutable = true, eval(expr))
        case Some(binding) =>
          if(!binding.mutable) { error(s"$name is immutable") }
          binding.value = eval(expr)
      }
      Value.None
    case Assign(Call(fn@Ident(_), params), body) if params.forall(_.isInstanceOf[Ident]) =>
      eval(Declare(Declare.Let, fn, Arrow(params.collect { case Ident(parameter) => parameter }, body)))
    case Assign(_, _) => `not implemented`
    case Declare(kind, Ident(name), expr) =>
      context.bindings.get(name) match {
        case Some(_) => error(s"$name is already declared")
        case None =>
          val mutable = kind match { case Declare.Var => true case Declare.Let => false }
          context.bindings += name -> new Binding(mutable, eval(expr))
          Value.None
      }
    case Declare(_, _, _) => `not implemented`
    case Print(expr) => println(Value.toString(eval(expr))); Value.None
    case Sequence(exprs) => eval(exprs)(context.child())
    case While(cond, body) =>
      var break = false
      while(!break && eval(cond).bool.value) {
        try eval(body)(context.child(inLoop = true))
        catch {
          case BreakException => break = true
          case ContinueException =>
        }
      }
      Value.None
    case BlockIf(cond, thenBlock, elseBlock) => if(eval(cond).bool.value) eval(thenBlock)(context.child()) else eval(elseBlock)(context.child())
    case Return(expr) =>
      if(!context.inFunction) error("invalid return")
      throw ReturnException(expr.map(eval))
    case Break =>
      if(!context.inLoop) error("invalid break")
      throw BreakException
    case Continue =>
      if(!context.inLoop) error("invalid continue")
      throw ContinueException
  }
}
