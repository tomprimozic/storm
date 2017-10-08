package storm.runtime


sealed trait Value {
  def bool: Value.Bool = Interpreter.error("expected a boolean")
  def int: Value.Integer = Interpreter.error("expected an integer")
  def function: Value.Function = Interpreter.error("expected a function")
  def record: Value.Record = Interpreter.error("expected a record")
}

object Value {
  case class Integer(value: Int) extends Value {
    override def int = this
  }
  case class Str(value: String) extends Value
  sealed abstract class Bool private[Value] (val value: Boolean) extends Value {
    override def bool = this
  }
  case object True extends Bool(true)
  case object False extends Bool(false)
  case object None extends Value
  case class Record(fields: Map[String, Value]) extends Value {
    override def record = this
  }

  trait Function extends Value {
    def apply(values: Seq[Value]): Value
    override def function = this
  }

  def apply(i: Int): Value = Integer(i)
  def apply(s: String): Value = Str(s)
  def apply(b: Boolean): Value = if(b) True else False

  def toString(v: Value): String = v match {
    case Integer(i) => i.toString
    case Str(s) => s
    case True => "true"
    case False => "false"
    case None => "none"
    case Record(fields) => s"{${ fields.map { case (field, value) => s"$field = ${ toString(value) }"}.mkString(", ") }"
  }
}

