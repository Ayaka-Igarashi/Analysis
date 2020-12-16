import java.io.PrintWriter
import java.util

import CommandStructure.CharacterToken
import Environment.DOCTYPEToken
import Main.{implement, pStateMap, txtOut3}
import StateProcessedStructure.pState
import com.jsonSchema.{Model, TestFormat}
import edu.stanford.nlp.io.IOUtils
import old.TestTest
import org.scalatest.FunSuite

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap

class Test extends FunSuite {

  txtOut3 = new PrintWriter("src/output3.txt")
  pStateMap = PreserveDefinition.read[ListMap[String, pState]]("src/definition.dat")

  test("example") {
    assert(1+2 === 3)
  }
  test("contentModelFlags.test") {
    val model = TestFormatter.format("src/test/testFile/contentModelFlags.test")
    val test = model.tests.get(4)
    val env = implement(test.input, "Data_state")
    println("implement : " + env.emitTokens)
    println("correct : " + test.output)
    println("implement : " + env.errorContent)
    println("correct : " + test.errors.get(0).code)
    assert(1+2 === 3)
    finishTest()
  }
  test("domjs.test") {
    val model = TestFormatter.format("src/test/testFile/test3.test")
    //val test = model.tests.get(1)
    //assert(1+2 === 3)
    val tests = model.tests.asScala.toList.slice(3,4)
    for (test <- tests) doTest(test)
    finishTest()
  }
  test("entities.test") {
    TestFormatter.format("src/test/testFile/entities.test")
    assert(1+2 === 3)
  }
  test("escapeFlag.test") {
    TestFormatter.format("src/test/testFile/escapeFlag.test")
    assert(1+2 === 3)
  }
  test("namedEntities.test") {
    TestFormatter.format("src/test/testFile/namedEntities.test")
    assert(1+2 === 3)
  }
  test("numericEntities.test") {
    TestFormatter.format("src/test/testFile/numericEntities.test")
    assert(1+2 === 3)
  }
  test("pendingSpecChanges.test") {
    TestFormatter.format("src/test/testFile/pendingSpecChanges.test")
    assert(1+2 === 3)
  }
  test("test1.test") {
    TestFormatter.format("src/test/testFile/test1.test")
    assert(1+2 === 3)
  }
  test("test2.test") {

    TestFormatter.format("src/test/testFile/test2.test")
    assert(1+2 === 3)
  }
  test("test3.test") {

    TestFormatter.format("src/test/testFile/test3.test")
    assert(1+2 === 3)
  }
  test("test4.test") {

    TestFormatter.format("src/test/testFile/test4.test")
    assert(1+2 === 3)
  }
  test("unicodeChars.test") {
    TestFormatter.format("src/test/testFile/unicodeChars.test")
    assert(1+2 === 3)
  }
  test("unicodeCharsProblematic.test") {
    TestFormatter.format("src/test/testFile/unicodeCharsProblematic.test")
    assert(1+2 === 3)
  }
  test("xmlViolation.test") {
    TestFormatter.format("src/test/testFile/xmlViolation.test")
    assert(1+2 === 3)
  }

  def finishTest() = {
    IOUtils.closeIgnoringExceptions(txtOut3)
  }

  def convertOutput(outputList: List[util.List[Any]]): List[Environment.Token] = {
    var convertedOutput: List[Environment.Token] = List()
    for (out <- outputList) {
      out.get(0) match {
        case "DOCTYPE"=>{
          val name = out.get(1) match {
            case s: String => s
            case _ => ""
          }
          val public_id = out.get(2) match {
            case s: String => s
            case _ => ""
          }
          val system_id = out.get(3) match {
            case s: String => s
            case _ => ""
          }
          val correctness = out.get(4) match {
            case b: Boolean => b
            case _ => false
          }
          convertedOutput :+= DOCTYPEToken(name, public_id, system_id, correctness)
        }
        case "StartTag" => {
          val name = out.get(1) match {
            case s: String => s
            case _ => ""
          }
          var attributeList: List[Environment.Attribute] = List()
          val attributes = out.get(2) match {
            case map: util.LinkedHashMap[String, String] => {
              val scalaMap = map.asScala.toList
              for (m <- scalaMap) {
                attributeList :+= Environment.Attribute(m._1, m._2)
              }
            }
            case _ =>
          }
          val flag = if (out.size() >= 4) out.get(3) match {
            case b: Boolean => b
            case _ => false
          }else false
          convertedOutput :+= Environment.tagToken(true, name, flag, attributeList)
        }
        case "EndTag" =>{
          val name = out.get(1) match {
            case s: String => s
            case _ => ""
          }
          convertedOutput :+= Environment.tagToken(false, name, false, List())
        }
        case "Comment" => {
          val data = out.get(1) match {
            case s: String => s
            case _ => ""
          }
          convertedOutput :+= Environment.commentToken(data)
        }
        case "Character" => {
          val data = out.get(1) match {
            case s: String => s
            case _ => ""
          }
          convertedOutput :+= Environment.characterToken(data)
        }
        case _ =>
      }
    }
    convertedOutput :+= Environment.endOfFileToken()
    convertedOutput
  }

  def getInitialState(initialStates: util.List[String]): String = {
    var initialState: String = "Data_state"
    if (initialStates != null) {
      initialState = initialStates.asScala.toList match {
        case state :: rst => state.replace(" ", "_")
        case Nil => "Data_state"
      }
    }
    initialState
  }

  def doTest(test: TestFormat) = {
    val initialState = getInitialState(test.initialStates)

    val outputList = test.output.asScala.toList
    val convertedOutput = convertOutput(outputList)
    //println(convertedOutput)

    val env = implement(test.input, initialState)
    //    println("implement : " + env.emitTokens)
    //    println("correct : " + test.output)

    var emitTokens = env.emitTokens
    for (correctOutput <- convertedOutput) {
      assert(emitTokens.head === correctOutput)
      emitTokens = emitTokens.tail
    }
  }
}
