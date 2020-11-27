object Environment {
  trait Value
  case class IntVal(i: Int) extends Value
  case class BoolVal(b: Boolean) extends Value
  case class StringVal(s: String) extends Value
  case class Val(var v: Any)

  class Env {
    // 現在の状態
    var currentState: String = null
    // 次に行く状態
    var nextState: String = "Data_state"

    // returnStateの値
    var returnState: String = null

    var currentInputCharacter: String = null
    var emitCharacterList: List[Char] = List()
    var errorContent: String = null

    // 入力文字列
    var inputText: String = null

    // その他
    var env: Map[String, Val] = Map()
  }

  trait Token
  case class DOCTYPE(name: String, public_identifier: String, system_identifier: String, force_quirks_flag: Boolean) extends Token
  case class tagToken(name: String, self_closing_flag: Boolean, attributes: List[Attribute]) extends Token

  class Attribute(name: String, value: String)
}
