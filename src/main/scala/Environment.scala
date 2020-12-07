import java.io.PrintWriter

object Environment {
  trait Value
  case class TokenVal(token: Token) extends Value
  case class AttributeVal(attribute: Attribute) extends Value
//  case class IntVal(i: Int) extends Value
//  case class BoolVal(b: Boolean) extends Value
//  case class StringVal(s: String) extends Value
  //case class Val(var v: Any)

  class Env {
    // 現在の状態
    var currentState: String = null
    // 次に行く状態
    var nextState: String = "Data_state"

    // returnStateの値
    var returnState: String = null
    var currentDOCTYPEToken: String = null
    var currentTagToken: String = null
    var commentToken: String = null

    var currentInputCharacter: InputCharacter = null
    var emitTokens: List[Token] = List()
    var errorContent: String = null

    // 入力文字列
    var inputText: String = null

    // その他
    var env: Map[String, Value] = Map()
    var mapID : Int = 0

    var corefMap: Map[Int, String] = Map()

    def setInputText(text: String) = { inputText = text }
    def setNextState(state: String) = { nextState = state }
    def addEmitToken(token: Token) = { emitTokens :+= token }
    def addMap(key: String, value: Value) = { env += (key -> value)}
    def getID(): Int = {val id = mapID; mapID += 1; id}
  }

  def printEnv(env: Env, write: PrintWriter, num: Int) = {
    write.println("------------------------------------------")
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
    write.println("map : ")
    for (m <- env.env) write.println(" " + m)
    write.println("------------------------------------------")
    write.println("")
  }

  trait Token
  case class DOCTYPEToken(name: String, public_identifier: String, system_identifier: String, force_quirks_flag: Boolean) extends Token
  case class tagToken(isStart: Boolean, name: String, self_closing_flag: Boolean, var attributes: List[Attribute]) extends Token
  case class tagToken_(isStart: Boolean, name: String, self_closing_flag: Boolean, var attributesKey: List[String]) extends Token
  case class commentToken(data: String) extends Token
  case class characterToken(data: String) extends Token
  case class endOfFileToken() extends Token

//  trait VariableOrToken
//  case class Variable(variable: String) extends VariableOrToken

  class Attribute(name: String, value: String)


  trait InputCharacter
  case class CharInput(char: Char) extends InputCharacter
  case class StrInput(string: String) extends InputCharacter
  case object EOF extends InputCharacter
}
