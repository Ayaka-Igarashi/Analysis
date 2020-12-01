import java.io.PrintWriter

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
    var currentDOCTYPEToken: DOCTYPEToken = null
    var currentTagToken: tagToken = null
    var commentToken: commentToken = null

    var currentInputCharacter: String = null
    var emitTokens: List[Token] = List()
    var errorContent: String = null

    // 入力文字列
    var inputText: String = null

    // その他
    var env: Map[String, Val] = Map()

    def setInputText(text: String) = { inputText = text }
    def setNextState(state: String) = { nextState = state }
    def addEmitToken(token: Token) = { emitTokens :+= token }
  }

  def printEnv(env: Env, write: PrintWriter, num: Int) = {
    write.println(num + " : =====================================")
    write.println("current state : " + env.currentState)
    write.println("next state : " + env.nextState)
    write.println("return state : " + env.returnState)
    write.println("current DOCTYPE Token : " + env.currentDOCTYPEToken)
    write.println("current tag Token : " + env.currentTagToken)
    write.println("comment Token : " + env.commentToken)
    write.println("current input character : " + env.currentInputCharacter)
    write.println("emit tokens : " + env.emitTokens)
    write.println("error content : " + env.errorContent)

    write.println("input text : " + env.inputText)

    write.println("")
  }

  trait Token
  case class DOCTYPEToken(name: String, public_identifier: String, system_identifier: String, force_quirks_flag: Boolean) extends Token
  case class tagToken(isStart: Boolean, name: String, self_closing_flag: Boolean, var attributes: List[Attribute]) extends Token
  case class commentToken(data: String) extends Token
  case class characterToken(data: String) extends Token
  case class endOfFileToken() extends Token

  class Attribute(name: String, value: String)
}
