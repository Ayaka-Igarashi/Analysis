import CommandStructure._
import Environment._
import StateProcessedStructure.pState

import scala.collection.immutable.ListMap

object Implement {

  var Env1: Map[String, Val] = Map(
    "end_tag_token" -> Val(tagToken("",false,List())),
    "nextState" -> Val("Data_state"),
    "inputCharStream" -> Val("aa")

  )
  //env updated ("currentState", env("nextState"))

  var eee: Env = new Env()

  // インタープリタ
  def interpret(env: Env, definition: ListMap[String, pState]): Env = { // env(環境)も引数に入れる,返り値もenvにする
    var newEnv: Env = env
    // 最初の処理
    val currentState = newEnv.nextState
    newEnv.currentState = currentState

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
      case Emit(characters) => {
        //env.emitCharacterList :+= characters // 途中
      }
      case Append(obj, to) =>
      case Error(error) => {
        newEnv.errorContent = error
        //println("Emit : "+error)
      }
      case Create(token) =>
      case Ignore(obj) =>
      case Flush() =>
      case Treat() =>
      case Start() =>
      case Multiply(obj, by) =>
      case Add(obj, to) =>
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
