package storm.runtime


sealed trait Value {
  def bool: Value.Bool = Interpreter.error("expected a boolean")
  def int: Value.Integer = Interpreter.error("expected an integer")
  def call(values: Seq[Value]): Value = Interpreter.error("not a function")
  def getField(name: String): Value = Interpreter.error(s"no field $name")
  def setField(name: String, value: Value): Unit = Interpreter.error(s"no field $name")
  def getItem(args: Seq[Value]): Value = Interpreter.error("cannot get item")
  def setItem(args: Seq[Value], value: Value): Unit = Interpreter.error("cannot set item")
  def iter(): Iterator[Value] = Interpreter.error("cannot be iterated")
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
  case class Record(fields: collection.mutable.Map[String, Value]) extends Value {
    override def getField(name: String) = fields(name)
    override def setField(name: String, value: Value): Unit = fields += name -> value
  }

  abstract class ClassValue(cls: Class) extends Value {
    override def getField(name: String): Value = cls.fields.getOrElse(name, Interpreter.error(s"no field $name")).get(this)
    override def setField(name: String, value: Value): Unit = cls.fields.getOrElse(name, Interpreter.error(s"no field $name")).set(this, value)
  }

  case class List(elements: collection.mutable.ArrayBuffer[Value]) extends ClassValue(Class.list) {
    def position(i: Int) = {
      val length = elements.length
      if(i >= length || i < -length) Interpreter.error("index out of bounds")
      else if(i >= 0) i else length + i
    }
    override def getItem(args: Seq[Value]) = args match { case Seq(Value.Integer(i)) => elements(position(i)) }
    override def setItem(args: Seq[Value], value: Value): Unit = args match { case Seq(Value.Integer(i)) => elements(position(i)) = value }
    override def iter() = elements.iterator
  }
  object List {
    def apply(elements: Value*): List = List(elements.to[collection.mutable.ArrayBuffer])
  }
  case class Range(from: Int, to: Int, inclusive: Boolean) extends Value {
    override def getField(name: String) = name match {
      case "from" => Value(from)
      case "to" => Value(to)
      case "inclusive" => Value(inclusive)
    }
    override def getItem(args: Seq[Value]) = args match { case Seq(Value.Integer(i)) => Value(from + i) }
    override def iter() = scala.Range(from, if(inclusive) to + 1 else to).iterator.map(Value(_))
  }

  trait Function extends Value {
    def apply(values: Seq[Value]): Value
    // this is silly, but it allows us to use the convenient Scala SAM syntax!
    override def call(values: Seq[Value]) = apply(values)
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
    case Record(fields) => s"{${ fields.map { case (field, value) => s"$field = ${ toString(value) }"}.mkString(", ") }}"
    case List(elements) => s"[${elements.map(toString).mkString(", ")}]"
    case Range(from, to, inclusive) => if(inclusive) s"$from...$to" else s"$from..$to"
  }
}


case class Class(fields: Map[String, Class.Field])

object Class {
  sealed abstract class Field {
    def get(obj: Value): Value
    def set(obj: Value, value: Value): Unit
  }

  trait Property extends Field {
    def set(obj: Value, value: Value): Unit = Interpreter.error("property cannot be set")
  }

  trait Method extends Field {
    def invoke(obj: Value, args: Seq[Value]): Value
    def bind(obj: Value) = new BoundMethod(this, obj)

    def get(obj: Value) = bind(obj)
    def set(obj: Value, value: Value): Unit = Interpreter.error("method cannot be set")
  }

  class BoundMethod(method: Method, obj: Value) extends Value.Function {
    override def apply(args: Seq[Value]) = method.invoke(obj, args)
  }

  def apply(methods: Map[String, Method], properties: Map[String, Property]): Class = {
    if(methods.keySet.intersect(properties.keySet).nonEmpty) throw new IllegalArgumentException("method and property names must differ")
    Class(methods ++ properties)
  }

  val list = Class(
    methods = Map(
      "append" -> { case (l: Value.List, Seq(value)) => l.elements.append(value); Value.None },
      "pop" -> { case (l: Value.List, Seq(Value.Integer(i))) => l.elements.remove(l.position(i)) },
      "clear" -> { case (l: Value.List, Seq()) => l.elements.clear(); Value.None },
      "insert" -> { case (l: Value.List, Seq(Value.Integer(i), value)) => l.elements.insert(l.position(i), value); Value.None }
    ), properties = Map(
      "length" -> { case l: Value.List => Value(l.elements.size) case _ => sys.error("impossible") }
  ))
}
