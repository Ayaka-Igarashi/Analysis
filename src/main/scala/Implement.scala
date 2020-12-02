import CommandStructure._
import Environment._
import Main.{txtOut2, txtOut3}
import StateProcessedStructure.pState

import scala.collection.immutable.ListMap

object Implement {

  var Env1: Map[String, Val] = Map(
    "end_tag_token_1" -> Val(tagToken(true, "",false,List()))
  )
  //env updated ("currentState", env("nextState"))

  var eee: Env = new Env()

  // インタープリタ
  def interpret(env: Env, definition: ListMap[String, pState]): Env = { // env(環境)も引数に入れる,返り値もenvにする
    var newEnv: Env = env
    // 最初の処理
    val currentState = newEnv.nextState
    newEnv.currentState = currentState
    newEnv.emitTokens = List()
    newEnv.currentInputCharacter = null
    newEnv.errorContent = null

    // 状態のマッチ
    definition.get(currentState) match {
      case Some(pState(_, prev, trans)) => {
        for (command <- prev) {
          newEnv = interpretCommand(newEnv, command)
        }
        // マッチング
        val commandList: List[Command] = characterMatching(newEnv.currentInputCharacter, trans)
        // Commandを1つずつ処理する
        for (command <- commandList) {
          newEnv = interpretCommand(newEnv, command)
        }
        txtOut3.println("command : "+ prev + " , " + commandList)
      }
      case None => println("undefined state error : " + currentState)
    }
    newEnv
  }

  def characterMatching(currentInputCharacter: String, trans: List[(String, List[Command])]): List[Command] = {
    trans match {
      case (character, comList) :: rst => {
        character match {
          case "ASCII upper alpha" => {
            val x = currentInputCharacter.codePointAt(0)
            if (x >= 0x0041 && x <= 0x005A) comList
            else characterMatching(currentInputCharacter, rst)
          }
          case "ASCII lower alpha" => {
            val x = currentInputCharacter.codePointAt(0)
            if (x >= 0x0061 && x <= 0x007A) comList
            else characterMatching(currentInputCharacter, rst)
          }
          case "ASCII alpha" => {
            val x = currentInputCharacter.codePointAt(0)
            if ((x >= 0x0041 && x <= 0x005A)||(x >= 0x0061 && x <= 0x007A)) comList
            else characterMatching(currentInputCharacter, rst)
          }
          case "Anything else" => comList
          case "EOF" => {
            if (currentInputCharacter == "[EOF]") comList
            else characterMatching(currentInputCharacter, rst)
          }
          case s => {
            val re =  "(U\\+[0-9A-F][0-9A-F][0-9A-F][0-9A-F])".r
            re.findFirstIn(s) match {
              case Some(unicode) => {
                if (currentInputCharacter.codePointAt(0) == Utility.unicodeToInt(unicode)) {
                  comList
                } else {
                  characterMatching(currentInputCharacter, rst)
                }
              }
              case None =>println("character error : " + s);characterMatching(currentInputCharacter, rst)
            }
          }
        }
      }
      case Nil => println("match_error"); List()
    }
  }

  def interpretCommand(env: Env, command: Command): Env = {
    var newEnv: Env = env
    command match {
      case Switch(state) => {
        newEnv.nextState = state
      }
      case Reconsume(state) => {
        newEnv.nextState = state
        newEnv.inputText = newEnv.currentInputCharacter + newEnv.inputText
        newEnv.currentInputCharacter = null
      }
      case Set(obj, to) =>
      case Consume(character) => {
        character match {
          case "next input character" => {
            if (newEnv.inputText.length > 0) {
              newEnv.currentInputCharacter = newEnv.inputText.head.toString
              newEnv.inputText = newEnv.inputText.tail
            } else {
              newEnv.currentInputCharacter = "[EOF]"
            }
          }
          case _ => {
            // 途中
            if (newEnv.inputText.substring(0, character.length) == character) {
              newEnv.inputText = newEnv.inputText.substring(character.length - 1)
              newEnv.currentInputCharacter = character
            } else {
              println("consume error")
            }
          }
        }
      }
      case Emit(character) => {
        if (character.contains("current tag token")) {
          newEnv.env.get(newEnv.currentTagToken) match {
            case Some(TokenVal(t)) => newEnv.addEmitToken(t)
            case None => println("cant find tag token")
          }
        } else if (character.contains("DOCTYPE token")) {
          newEnv.env.get(newEnv.currentDOCTYPEToken) match {
            case Some(TokenVal(t)) => newEnv.addEmitToken(t)
            case None => println("cant find DOCTYPE token")
          }
        } else if (character.contains("comment token")) {
          newEnv.env.get(newEnv.commentToken) match {
            case Some(TokenVal(t)) => newEnv.addEmitToken(t)
            case None => println("cant find comment token")
          }
        } else if (character.contains("end_of_file")) {
          newEnv.addEmitToken(endOfFileToken())
        } else if ("""(.+) (as a character token)""".r.matches(character)){
          val re2 = """(.+) (as a character token)""".r
          val re2(c, _) = character
          if (c.contains("current input character")) {
            newEnv.addEmitToken(characterToken(env.currentInputCharacter))
          } else {
            newEnv.addEmitToken(characterToken(character))
          }
        } else {
          newEnv.addEmitToken(characterToken(character))
          txtOut3.println("emit error" + character)
        }
      }
      case Append(obj, to) =>
      case Error(error) => {
        newEnv.errorContent = error
        //println("ErrorCode : "+error)
      }
      case Create(token) => {
        if (token.contains("start tag token")) {
          val key = "start_tag_token_" + newEnv.getID()
          newEnv.addMap(key, TokenVal(tagToken(true, "", false, List())))
          newEnv.currentTagToken = key
        } else if (token.contains("end tag token")) {
          val key = "end_tag_token_" + newEnv.getID()
          newEnv.addMap(key, TokenVal(tagToken(false, "", false, List())))
          newEnv.currentTagToken = key
        } else if (token.contains("DOCTYPE token")) {
          val key = "DOCTYPE_token_" + newEnv.getID()
          newEnv.addMap(key, TokenVal(DOCTYPEToken("", null, null, false)))
          newEnv.currentDOCTYPEToken = key
        } else if (token.contains("comment token")) {
          val key = "comment_token_" + newEnv.getID()
          newEnv.addMap(key, TokenVal(commentToken("")))
          newEnv.commentToken = key
        } else {
          println("create error" + token)
        }
      }
      case Ignore(obj) => println("ignore : " + obj) // 何もしない
      case Flush() => {
        val flushCommand =  If(IsEqual("character reference", "consumed as part of an attribute"),
                              List(Append("code point from the buffer", "current attribute's value")),
                               List(Emit("code point as a character token")))
        newEnv = interpretCommand(newEnv, flushCommand)
      }
      case Treat() => {
        // AnithingElseのコマンド実行する
      }
      case Start() => {
        // currentTagTokenに新しい属性を追加する
        //newEnv.currentTagToken.attributes :+= new Attribute(null, null)
      }
      case Multiply(obj, by) => {
        obj match {
          case "character reference code" => {
            val mutiNum = by.toInt
          }
          case _ =>
        }
      }
      case Add(obj, to) => {
        obj match {
          case "character reference code" => {
          }
          case _ =>
        }
      }
      case If(bool, t, f) => {
        var comList: List[Command] = null
        if (implementBool(bool)) comList = t else comList = f
        for (c <- comList) newEnv = interpretCommand(newEnv, c)
      }
      case IF_(_) | OTHERWISE_() => println("IF not converted error : " + command)
      case _ => println("undefined command error : " + command)
    }
    newEnv
  }

  def implementBool(bool: Bool): Boolean = { // env(環境)も引数に入れる
    bool match {
      case And(b1, b2) => implementBool(b1) && implementBool(b2)
      case Or(b1, b2) => implementBool(b1) || implementBool(b2)
      case Not(b) => !implementBool(b)
        // 途中
      case IsEqual(a, b) => true
      case IsExist(a) => true
      case UNDEF(str) => println("undefined bool : " + bool);false
      case _ => println("undefined bool error : " + bool);false
    }
  }
}
