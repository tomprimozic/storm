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
      "math" -> Value.Record(collection.mutable.Map(
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

  class Closure(params: Seq[String], body: Node)(implicit context: Context) extends Value.Function {
    val closure = context.copy()
    override def apply(values: Seq[Value]) = {
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
    case Field(expr, field) => eval(expr).getField(field)
    case Call(fn, exprs) => eval(fn).call(exprs.map(eval))
    case Record(fields) =>
      val record = collection.mutable.Map[String, Value]()
      fields.foreach { case (field, expr) =>
        if(record.contains(field)) error(s"duplicate field $field")
        record += field -> eval(expr)
      }
      Value.Record(record)
    case List(elements) =>
      val values = collection.mutable.ArrayBuffer[Value]()
      elements.foreach {
        case Spread(expr) => values ++= eval(expr).iter()
        case expr => values += eval(expr)
      }
      Value.List(values)
    case Item(expr, args) => eval(expr).getItem(args.map(eval))
    case Arrow(params, body) => new Closure(params, body)
    case Binary(left, assignOp, right) if assignOp.endsWith("=") =>
      val op = assignOp.dropRight(1)
      left match {
        case Ident(_) => eval(Assign(left, Binary(left, op, right)))
        case Field(expr, field) =>
          val obj = eval(expr)
          val leftValue = obj.getField(field)
          val rightValue = eval(right)
          val result = context.value(op).call(Seq(leftValue, rightValue))
          obj.setField(field, result)
        case Item(expr, args) =>
          val obj = eval(expr)
          val argValues = args.map(eval)
          val leftValue = obj.getItem(argValues)
          val rightValue = eval(right)
          val result = context.value(op).call(Seq(leftValue, rightValue))
          obj.setItem(argValues, result)
        case _ => `not implemented`
      }
      Value.None
    case Binary(left, op, right) => eval(Call(Ident(op), Seq(left, right)))
    case Unary(op, expr) => eval(Call(Ident(op), Seq(expr)))
    case Cmp(ops, exprs) =>
      assert(ops.nonEmpty && exprs.size == ops.size + 1)
      @tailrec
      def loop(previous: Value, ops: Seq[String], exprs: Seq[Node]): Value = {
        if(ops.isEmpty) Value.True else {
          val next = eval(exprs.head)
          val op = context.value(ops.head)
          val result = op.call(Seq(previous, next)).bool.value
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
    case Assign(call@Call(Ident(_), params), body) => eval(Function(call, Seq(body)))
    case Assign(Field(obj, field), expr) =>
      val value = eval(expr)
      eval(obj).setField(field, value)
      Value.None
    case Assign(Item(obj, args), expr) =>
      val value = eval(expr)
      eval(obj).setItem(args.map(eval), value)
      Value.None
    case Assign(_, _) => `not implemented`
    case Declare(kind, Ident(name), expr) =>
      if(context.bindings.contains(name)) error(s"$name is already declared")
      val mutable = kind match { case Declare.Var => true case Declare.Let => false }
      context.bindings += name -> new Binding(mutable, eval(expr))
      Value.None
    case Declare(_, _, _) => `not implemented`
    case Function(Call(Ident(fn), parameters), body) =>
      val params = parameters.map { case Ident(name) => name case _ => `not implemented` }
      if(context.bindings.contains(fn)) error(s"$fn is already declared")
      val binding = new Binding(mutable = false, null)
      context.bindings += fn -> binding
      binding.value = new Closure(params, Sequence(body))
      Value.None
    case Function(_, _) => `not implemented`
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
    case ForIn(Ident(name), expr, body) =>
      val iterator = eval(expr).iter()
      var break = false
      while(!break && iterator.hasNext) {
        val loopContext = context.child(inLoop = true)
        loopContext.bindings += name -> new Binding(mutable = false, iterator.next())
        try eval(body)(loopContext)
        catch {
          case BreakException => break = true
          case ContinueException =>
        }
      }
      Value.None
    case Return(expr) =>
      if(!context.inFunction) error("invalid return")
      throw ReturnException(expr.map(eval))
    case Break =>
      if(!context.inLoop) error("invalid break")
      throw BreakException
    case Continue =>
      if(!context.inLoop) error("invalid continue")
      throw ContinueException
    case Range(from, to, inclusive) =>
      val fromValue = eval(from)
      val toValue = eval(to)
      Value.Range(fromValue.int.value, toValue.int.value, inclusive)
    case Spread(_) => error("invalid spread")
  }
}
