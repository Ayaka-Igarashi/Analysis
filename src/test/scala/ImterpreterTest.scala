import Main.{implement, pStateMap, txtOut3, txtOut4}
import StateProcessedStructure.pState
import org.scalatest.FunSuite

import Implement._
import CommandStructure._
import Environment._

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap

class ImterpreterTest extends FunSuite {
  pStateMap = PreserveDefinition.read[ListMap[String, pState]]("src/definition.dat")
  var count: Int = 0
  var correctCount: Int = 0
  var correctCountError: Int = 0

  test("test Switch") {
    // 正しく遷移するか
    val env = new Env()
    val newEnv = interpretCommand(env, Switch(StateName("RAWTEXT_state")))
    assert(newEnv.nextState === StateVal("RAWTEXT_state"))

    // Return stateに正しく遷移するか
    val env2 = new Env()
    env2.returnState = StateVal("Script_data_state")
    val newEnv2 = interpretCommand(env2, Switch(ReturnState))
    assert(newEnv2.nextState === StateVal("Script_data_state"))
  }
  test("test Reconsume") {
    // 正しく遷移するか
    val env = new Env()
    env.currentInputCharacter = EOFVal
    val newEnv = interpretCommand(env, Reconsume(StateName("RAWTEXT_state")))
    assert(newEnv.nextState === StateVal("RAWTEXT_state"))

    // Return stateに正しく遷移するか
    val env2 = new Env()
    env2.currentInputCharacter = EOFVal
    env2.returnState = StateVal("Script_data_state")
    val newEnv2 = interpretCommand(env2, Reconsume(ReturnState))
    assert(newEnv2.nextState === StateVal("Script_data_state"))
  }
  test("test Emit") {
    // 正しくcharactertokrnを排出するか
    val env = new Env()
    val newEnv = interpretCommand(env, Emit(CharacterToken(CChar('a'))))
    assert(newEnv.emitTokens === List(characterToken("a")))

    // current input characterを正しく輩出するか
    val env2 = new Env()
    env2.currentInputCharacter = CharVal('b')
    val newEnv2 = interpretCommand(env2, Emit(CurrentInputCharacter))
    assert(newEnv2.emitTokens === List(characterToken("b")))
  }
  test("test Set") {
    // return stateが正しく代入されているか
    val env = new Env()
    val newEnv = interpretCommand(env, Set(IReturnState, StateName("Script_data_state")))
    assert(newEnv.returnState === StateVal("Script_data_state"))

    // tagの名前が正しく代入されているか
    val env2 = new Env()
    env2.currentInputCharacter = CharVal('b')
    env2.currentTagToken = "1"
    env2.addMap("1", TokenVal(tagToken_(true, null, false, List())))
    val newEnv2 = interpretCommand(env2, Set(INameOf(ICurrentTagToken), CString("br")))
    assert(newEnv2.map.get(newEnv2.currentTagToken) === Some(TokenVal(tagToken_(true, "br", false, List())) ))
  }
  test("test Consume") {

  }
  test("test AppendTo") {

  }
  test("test Error") {

  }
  test("test Create") {

  }
  test("test Ignore") {
    val env = new Env()
    val newEnv = interpretCommand(env, Ignore(""))
    assert(env === newEnv)
  }
  test("test FlushCodePoint") {

  }
  test("test TreatAsAnythingElse") {

  }
  test("test StartNewAttribute") {

  }
  test("test MultiplyBy") {

  }
  test("test AddTo") {

  }
  test("test If") {

  }

}
