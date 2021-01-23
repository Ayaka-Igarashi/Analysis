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
    // Consume
    val env = new Env()
    env.inputText = "abcde"
    val newEnv = interpretCommand(env, Consume(NextInputCharacter(1)))
    assert(newEnv.inputText === "bcde")
    assert(newEnv.currentInputCharacter === CharVal('a'))

    // Consume
    val env2 = new Env()
    env2.inputText = "abcde"
    val newEnv2 = interpretCommand(env2, Consume(CString("abc")))
    assert(newEnv2.inputText === "de")
    assert(newEnv2.currentInputCharacter === StringVal("abc"))
  }
  test("test AppendTo") {
    // temporary bufferが正しく追加されているか
    val env = new Env()
    env.temporaryBuffer = "ab"
    val newEnv = interpretCommand(env, AppendTo(CString("cd"), ITemporaryBuffer))
    assert(newEnv.temporaryBuffer === "abcd")

    // tagの名前が正しく追加されているか
    val env2 = new Env()
    env2.currentTagToken = "1"
    env2.addMap("1", TokenVal(tagToken_(true, "ab", false, List())))
    val newEnv2 = interpretCommand(env2, AppendTo(CString("cd"), INameOf(ICurrentTagToken)))
    assert(newEnv2.map.get(newEnv2.currentTagToken) === Some(TokenVal(tagToken_(true, "abcd", false, List())) ))
  }
  test("test Error") {
    // 正しくError出すか
    val env = new Env()
    val newEnv = interpretCommand(env, Error("unexpected_null_character parse error"))
    assert(newEnv.errorContent === List("unexpected_null_character parse error"))
  }
  test("test Create") {
    // 正しくcreateするか
    val env = new Env()
    val newEnv = interpretCommand(env, Create(NewStartTagToken, "1"))
    assert(newEnv.map.get(newEnv.currentTagToken) === Some(TokenVal(tagToken_(true, null, false, List())) ))
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
    // 正しく遷移するか
    val env = new Env()
    val newEnv = interpretCommand(env, If(CharacterReferenceConsumedAsAttributeVal(),  List(Switch(StateName("RAWTEXT_state"))), List(Switch(StateName("Script_data_state")))))
    assert(newEnv.nextState === StateVal("Script_data_state"))

    // Return stateに正しく遷移するか
    val env2 = new Env()
    env2.returnState = StateVal("Attribute_value_unquoted_state")
    val newEnv2 = interpretCommand(env2, If(CharacterReferenceConsumedAsAttributeVal(),  List(Switch(StateName("RAWTEXT_state"))), List(Switch(StateName("Script_data_state")))))
    assert(newEnv2.nextState === StateVal("RAWTEXT_state"))
  }

  test("test bool") {
    val env = new Env()
    env.returnState = StateVal("Attribute_value_double_quoted_state")
    val b1 = implementBool(env, CharacterReferenceConsumedAsAttributeVal())
    assert(b1._1 === true)
  }

  test("test character matching") {
    val trans: List[(String, List[Command])]
        = List(("U+003C LESS-THAN SIGN (<)", List(Switch(StateName("RCDATA_state")))),
                ("ASCII alpha", List(Switch(StateName("RAWTEXT_state")))),
                ("ASCII digit", List(Switch(StateName("Script_data_state")))),
                ("Anything else", List(Switch(StateName("PLAINTEXT_state")))))
    val c1 = characterMatching(CharVal('a'), trans)
    val c2 = characterMatching(CharVal('<'), trans)
    val c3 = characterMatching(CharVal('8'), trans)
    val c4 = characterMatching(CharVal('\u0000'), trans)
    assert(c1 === List(Switch(StateName("RAWTEXT_state"))))
    assert(c2 === List(Switch(StateName("RCDATA_state"))))
    assert(c3 === List(Switch(StateName("Script_data_state"))))
    assert(c4 === List(Switch(StateName("PLAINTEXT_state"))))
  }

  test("test commandValueToValue") {
    val env = new Env()
    env.inputText = "abc"
    val v1 = commandValueToValue(NextInputCharacter(1), env)
    assert(v1._1 === CharVal('a'))
  }

  test("test ") {
    assert(1 === 1)
  }

}
