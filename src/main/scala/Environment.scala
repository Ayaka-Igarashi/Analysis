object Environment {
  trait Value
  case class IntVal(i: Int) extends Value
  case class BoolVal(b: Boolean) extends Value
  case class StringVal(s: String) extends Value
  case class Val(var v: Any)
}
