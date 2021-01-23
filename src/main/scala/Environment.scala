import java.io.PrintWriter

object Environment {
  trait Value
  case class TokenVal(token: Token) extends Value
  case class AttributeVal(attribute: Attribute) extends Value

  case class IntVal(int: Long) extends Value
  case class CharVal(char: Char) extends Value
  case class StringVal(string: String) extends Value
  case object EOFVal extends Value
  case class StateVal(state: String) extends Value
  case class BoolVal(b: Boolean) extends Value

  def Add(v1: Value, v2: Value): Value = {
    v1 match {
      case StringVal(s1) => {
        v2 match {
          case StringVal(s2) => StringVal(s1 + s2)
          case CharVal(c2) => StringVal(s1 + c2)
          case IntVal(i2) => StringVal(s1 + i2)
          case EOFVal => StringVal(s1)
          case _ => println("AddValueError");StringVal(s1)
        }
      }
      case CharVal(c1) => {
        v2 match {
          case StringVal(s2) => StringVal(c1 + s2)
          case CharVal(c2) => StringVal(c1.toString + c2.toString)
          case IntVal(i2) => IntVal(c1 + i2)
          case EOFVal => CharVal(c1)
          case _ => println("AddValueError");CharVal(c1)
        }
      }
      case IntVal(i1) => {
        v2 match {
          case StringVal(s2) => StringVal(i1 + s2)
          case CharVal(c2) => IntVal(i1 + c2)
          case IntVal(i2) => IntVal(i1 + i2)
          case _ => println("AddValueError");IntVal(i1)
        }
      }
      case EOFVal => {
        v2 match {
          case StringVal(s2) => StringVal(s2)
          case CharVal(c2) => CharVal(c2)
          case EOFVal => EOFVal
          case _ => println("AddValueError"); EOFVal
        }
      }
      case _ => println("AddValueError");v1
    }
  }
  def Head(v: Value): Value = {
    v match {
      case StringVal(s) => s.headOption match {
        case Some(c) => CharVal(c)
        case None => EOFVal
      }
      case CharVal(c) => CharVal(c)
      case _ => null
    }
  }
  def Tail(v: Value): Value = {
    null
//    v match {
//      case StringVal(s) => s.headOption match {
//        case Some(c) => CharVal(c)
//        case None => EOFVal
//      }
//      case CharVal(c) => CharVal(c)
//      case _ => null
//    }
  }
//  def Add(v1: CharVal, v2: StringVal): StringVal = {
//    v1 match {
//      case CharVal(c) => v2 match {
//        case StringVal(s) => StringVal(c + s)
//      }
//    }
//  }
//  def StringAdd(v1: Value, v2: Value): StringVal = {
//    v1 match {
//      case StringVal(s1) => {
//        v2 match {
//          case StringVal(s2) => StringVal(s1 + s2)
//          case CharVal(c2) => StringVal(s1 + c2)
//          case IntVal(i2) => StringVal(s1 + i2)
//          case EOFVal => StringVal(s1)
//          case _ => println("AddValueError");null
//        }
//      }
//      case CharVal(c1) => {
//        v2 match {
//          case StringVal(s2) => StringVal(c1 + s2)
//          case CharVal(c2) => StringVal(c1.toString + c2.toString)
//          case EOFVal => StringVal(c1.toString)
//          case _ => println("AddValueError");null
//        }
//      }
//      case IntVal(i1) => {
//        v2 match {
//          case StringVal(s2) => StringVal(i1 + s2)
//          case CharVal(c2) => StringVal((i1 + c2).toString)
//          case IntVal(i2) => StringVal((i1 + i2).toString)
//          case _ => println("AddValueError");null
//        }
//      }
//      case EOFVal => v2 match {
//        case StringVal(s2) => StringVal(s2)
//        case CharVal(c2) => StringVal(c2.toString)
//        case EOFVal => StringVal("")
//        case _ => null
//      }
//      case _ => println("AddValueError");null
//    }
//  }

  class Env {
    // 現在の状態
    var currentState: StateVal = null
    // 次に行く状態
    var nextState: StateVal = StateVal("Data_state")

    // returnStateの値
    var returnState: StateVal = null
    var temporaryBuffer: String = ""
    var characterReferenceCode: Long = 0
    var currentDOCTYPEToken: String = null
    var currentTagToken: String = null
    var commentToken: String = null
    var currentAttribute: String = null
    var lastStartTagName: String = null

    var currentInputCharacter: Value = null
    var emitTokens: List[Token] = List()
    var errorContent: List[String] = List()

    // 入力文字列
    var inputText: String = null

    // その他
    var map: Map[String, Value] = Map()
    var mapID : Int = 0

    def setInputText(text: String) = { inputText = (text) }
    def setNextState(state: StateVal) = { nextState = state }
    def addEmitToken(token: Token) = { emitTokens :+= token }
    def addMap(key: String, value: Value) = { map += (key -> value)}
    def getID(): Int = {val id = mapID; mapID += 1; id}
  }

  def printEnv(env: Env, write: PrintWriter, num: Int) = {
    write.println("------------------------------------------")
    write.println("current state : " + env.currentState)
    write.println("next state : " + env.nextState)
    write.println("return state : " + env.returnState)
    write.println("current DOCTYPE Token : " + env.currentDOCTYPEToken)
    write.println("current tag Token : " + env.currentTagToken)
    write.println("current attribute : " + env.currentAttribute)
    write.println("comment Token : " + env.commentToken)
    write.println("temporary buffer : " + env.temporaryBuffer)
    write.println("character reference code : " + env.characterReferenceCode)
    write.println("last start tag name : " + env.lastStartTagName)
    write.println("current input character : " + env.currentInputCharacter)
    write.println("emit tokens : " + env.emitTokens)
    write.println("error content : " + env.errorContent)

    write.println("input text : " + env.inputText)
    write.println("map : ")
    for (m <- env.map) write.println(" " + m)
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
//  case class startTagToken(name: String, self_closing_flag: Boolean, var attributes: List[Attribute]) extends Token
//  case class endTagToken(name: String, self_closing_flag: Boolean, var attributes: List[Attribute]) extends Token

  case class Attribute(name: String, value: String)

//  trait InputCharacter
//  case class CharInput(char: Char) extends InputCharacter
//  case class StrInput(string: String) extends InputCharacter
//  case object EOF extends InputCharacter
}
