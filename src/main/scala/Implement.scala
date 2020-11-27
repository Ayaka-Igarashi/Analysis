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
//        var currentInputCharacter = newEnv.inputText.head.toString
//        newEnv.inputText = newEnv.inputText.tail
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
          case "EOF" => { // 途中
            comList
          }
          case _ => {
            // 途中
            if (character == currentInputCharacter) {
              comList
            } else {
              characterMatching(currentInputCharacter, rst)
            }
          }
        }
      }
      case Nil => println("match_error"); List()
    }
  }

  def interpretCommand(env: Env, command: Command): Env = { // env(環境)も引数に入れる,返り値もenvにする
    command match {
      case Switch(state) => {
        env.nextState = state
      }
      case Reconsume(state) => {
        env.nextState = state
        env.inputText = env.currentInputCharacter + env.inputText
        env.currentInputCharacter = null
      }
      case Set(obj, to) =>
      case Consume(character) => {
        if (env.inputText.substring(0, character.length) == character) {
          env.inputText = env.inputText.substring(character.length - 1)
          env.currentInputCharacter = character
        } else {
          println("consume error")
        }
      }
      case Emit(characters) => {
        //env.emitCharacterList :+= characters // 途中
      }
      case Append(obj, to) =>
      case Error(error) => {
        env.errorContent = error
        //println("Emit : "+error)
      }
      case Create(token) =>
      case Ignore(obj) =>
      case Flush() =>
      case Treat() =>
      case Start() =>
      case Multiply(obj, by) =>
      case Add(obj, to) =>
      case If(bool, t, f) =>
      case IF_(_) | OTHERWISE_() => println("IF not converted error : " + command)
      case _ => println("undefined command error : " + command)
    }
    env
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
