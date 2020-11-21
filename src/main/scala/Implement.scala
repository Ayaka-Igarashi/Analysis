import CommandStructure._
import Environment._
import StateProcessedStructure.pState

import scala.collection.immutable.ListMap

object Implement {

  var currentState: String = "Data_state"
  var nextState: String = "Data_state"
  var inputCharStream: String = "aa"

  var Env: Map[String, Val] = Map(
    "currentState" -> Val("Data_state"),
    "nextState" -> Val("Data_state"),
    "inputCharStream" -> Val("aa")

  )

  // インタープリタ
  def interpret(env: Map[String, Val], definition: ListMap[String, pState]) = { // env(環境)も引数に入れる,返り値もenvにする
    // 最初の処理書く
    env updated ("currentState", env("nextState"))
    currentState = nextState

    // 状態のマッチ
    definition.get(currentState) match {
      case Some(pState(name, prev, trans)) => {
        var currentInputCharacter = inputCharStream.head.toString
        inputCharStream = inputCharStream.tail

        var commandList: List[Command] = null
        trans.get(currentInputCharacter) match {
          case Some(comList) => {
            commandList = comList
          }
          case None => { // AnythingElse

          }
        }

        // Commandを1つずつ処理する
        for (command <- commandList) {
          interpretCommand(command)
        }

      }
      case None => println("undefined state error : " + currentState)
    }
  }

  def interpretCommand(command: Command) = { // env(環境)も引数に入れる,返り値もenvにする
    command match {
      case Switch(state) => {
        nextState = state
      }
      case Reconsume(state) =>
      case Set(obj, to) =>
      case Consume(character) =>
      case Emit(characters) =>
      case Append(obj, to) =>
      case Error(error) =>
      case Create(token) =>
      case Ignore(obj) =>
      case Flush() =>
      case Treat() =>
      case Start() =>
      case Multiply(obj, by) =>
      case Add(obj, to) =>
      case If(bool, t, f) =>
      case _ => println("undefined command error : " + command)
    }
  }
}
