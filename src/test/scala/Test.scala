import java.io.PrintWriter
import java.util

import CommandStructure.CharacterToken
import Environment.DOCTYPEToken
import Main.{implement, pStateMap, txtOut4, txtOut3}
import StateProcessedStructure.pState
import com.jsonSchema.{Model, TestFormat}
import edu.stanford.nlp.io.IOUtils
import old.TestTest
import org.scalatest.FunSuite

import scala.collection.JavaConverters._
import scala.collection.immutable.ListMap

class Test extends FunSuite {
  pStateMap = PreserveDefinition.read[ListMap[String, pState]]("src/definition.dat")
  var count: Int = 0
  var correctCount: Int = 0
  var correctCountError: Int = 0

  test("example") {
    assert(1+2 === 3)
  }
  test("contentModelFlags.test") {
    startTest()
    txtOut4.println("contentModelFlags")
    val model = TestFormatter.format("src/test/testFile/contentModelFlags.test")
    val tests = model.tests.asScala.toList
    for (test <- tests) doTest(test)
    println("correct : " + correctCount + "/" + count)
    finishTest()
  }
  test("domjs.test") {
    startTest()
    testFormat("domjs")
    testFormat("entities")
    testFormat("escapeFlag")
    testFormat("namedEntities")
    testFormat("numericEntities")
    testFormat("pendingSpecChanges")
    testFormat("test1")
    testFormat("test2")
    testFormat("test3")
    testFormat("test4")
    testFormat("unicodeChars")
    testFormat("unicodeCharsProblematic")
    testFormat("xmlViolation")

//    txtOut4.println("domjs")
//    val model = TestFormatter.format("src/test/testFile/domjs.test")
//    val tests = model.tests.asScala.toList//.slice(3,4)
//    for (test <- tests) doTest(test)
//    println("correct : " + correctCount + "/" + count)
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

  def testFormat(file: String) = {
    count = 0
    correctCount = 0
    correctCountError = 0
    //startTest()
    txtOut4.println(file)
    val model = TestFormatter.format("src/test/testFile/" + file + ".test")
    val tests = model.tests.asScala.toList//.slice(3,4)
    for (test <- tests) doTest(test)
    println(file + " => correct : " + correctCount + "/" + count)
    println(file + " => correctError : " + correctCountError + "/" + count)
    //finishTest()
  }

  def startTest() = {
    txtOut3 = new PrintWriter("src/output3.txt")
    txtOut4 = new PrintWriter("src/output4.txt")
  }

  def finishTest() = {
    IOUtils.closeIgnoringExceptions(txtOut4)
    IOUtils.closeIgnoringExceptions(txtOut3)
  }

  def convertOutput(outputList: List[util.List[Any]], doubleEscaped: Boolean): List[Environment.Token] = {
    var convertedOutput: List[Environment.Token] = List()
    for (out <- outputList) {
      out.get(0) match {
        case "DOCTYPE"=>{
          val name = out.get(1) match {
            case s: String => {
              if (doubleEscaped) "\\\\u[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]".r.replaceAllIn(s, m => Integer.parseInt(m.toString().tail.tail, 16).toChar.toString)
              else s
            }
            case null => null
            case _ => ""
          }
          val public_id = out.get(2) match {
            case s: String => {
              if (doubleEscaped) "\\\\u[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]".r.replaceAllIn(s, m => Integer.parseInt(m.toString().tail.tail, 16).toChar.toString)
              else s
            }
            case null => null
            case _ => ""
          }
          val system_id = out.get(3) match {
            case s: String => {
              if (doubleEscaped) "\\\\u[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]".r.replaceAllIn(s, m => Integer.parseInt(m.toString().tail.tail, 16).toChar.toString)
              else s
            }
            case null => null
            case _ => ""
          }
          val correctness = out.get(4) match {
            case b: Boolean => !b
            case _ => false
          }
          convertedOutput :+= DOCTYPEToken(name, public_id, system_id, correctness)
        }
        case "StartTag" => {
          val name = out.get(1) match {
            case s: String => {
              if (doubleEscaped) "\\\\u[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]".r.replaceAllIn(s, m => Integer.parseInt(m.toString().tail.tail, 16).toChar.toString)
              else s
            }
            case null => null
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
            case s: String => {
              if (doubleEscaped) "\\\\u[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]".r.replaceAllIn(s, m => Integer.parseInt(m.toString().tail.tail, 16).toChar.toString)
              else s
            }
            case null => null
            case _ => ""
          }
          convertedOutput :+= Environment.tagToken(false, name, false, List())
        }
        case "Comment" => {
          val data = out.get(1) match {
            case s: String => {
              if (doubleEscaped) "\\\\u[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]".r.replaceAllIn(s, m => Integer.parseInt(m.toString().tail.tail, 16).toChar.toString)
              else s
            }
            case null => null
            case _ => ""
          }
          convertedOutput :+= Environment.commentToken(data)
        }
        case "Character" => {
          val data = out.get(1) match {
            case s: String => {
              if (doubleEscaped) "\\\\u[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]".r.replaceAllIn(s, m => Integer.parseInt(m.toString().tail.tail, 16).toChar.toString)
              else s
            }
            case null => null
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

  def getInitialStates(initialStates: util.List[String]): List[String] = {
    if (initialStates == null) List("Data_state")
    else {
      val initialStates_scalaList = initialStates.asScala.toList
      if (initialStates_scalaList == Nil) List("Data_state")
      else {
        var newStates: List[String] = List()
        for (state <- initialStates_scalaList) newStates +:= state.replace(" ", "_")
        newStates
      }
    }
//    var initialState: List[String] = List()
//    if (initialStates != null) {
//      initialState = initialStates.asScala.toList match {
//        case state :: rst => List(state.replace(" ", "_"))
//        case Nil => initialState
//      }
//    }
//    initialState
  }

  def doTest(test: TestFormat) = {
    val initialStates = getInitialStates(test.initialStates)

    var inputText = test.input
    val doubleEscaped = (test.doubleEscaped != null && test.doubleEscaped)
    if (doubleEscaped) {
      inputText = "\\\\u[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]".r.replaceAllIn(inputText, m => Integer.parseInt(m.toString().tail.tail, 16).toChar.toString)
    }

    val lastStartTagName = test.lastStartTag

    val outputList = test.output.asScala.toList
    val convertedOutput = convertOutput(outputList, doubleEscaped)

    for (initialState <- initialStates) {
      val env = implement(inputText, initialState, lastStartTagName)

      var isCorrect = true
      var emitTokens = Main.combineCharacterToken(env.emitTokens)
      for (correctOutput <- convertedOutput) {
        //assert(emitTokens.head === correctOutput)
        if (emitTokens != Nil) {
          emitTokens.head match {
            case Environment.tagToken(false, n,_,_) => {
              if (!(Environment.tagToken(false, n, false, List()) == correctOutput)) {
                isCorrect = false
              }
            }
            case _ => {
              if (!(emitTokens.head == correctOutput)) {
                isCorrect = false
              }
            }
          }
          emitTokens = emitTokens.tail
        }
        else {
          isCorrect = false
        }
      }
      count += 1
      if (isCorrect) correctCount += 1
      txtOut4.println("testnum :" + count + ";")
      txtOut4.println("state : " + initialState)
      txtOut4.println("lastStartTagName : " + lastStartTagName)
      txtOut4.println("input : \"" + test.input + "\"")
      txtOut4.println("implement : " + Main.combineCharacterToken(env.emitTokens))
      txtOut4.println("correct : " + convertedOutput)

      txtOut4.println("implement_error : " + env.errorContent)
      val correctError = if (test.errors == null) List() else test.errors.asScala.toList
      txtOut4.print("correct_error : ")
      for (error <- correctError) txtOut4.print(" " + error.code)
      txtOut4.println("")

      val errorIsCorrect = doErrorTest(test.errors, env.errorContent)
      txtOut4.println("errorText -> " + errorIsCorrect)
      if (errorIsCorrect) correctCountError += 1

      txtOut4.println(" -> " + isCorrect + "\n")
    }

  }

  def doErrorTest(correct: java.util.List[com.jsonSchema.Error], imp: List[String]): Boolean = {
    val correctError = if (correct == null) List() else correct.asScala.toList
    var impErrorList = imp
    var isEqual = true
    for (error <- correctError) {
      if (impErrorList != Nil) {
        if (!error.code.contains("-in-input-stream")) {
          if (error.code+ " parse error" != impErrorList.head.replace("_", "-")) {
            txtOut4.println(error.code)
            isEqual = false
          }
          impErrorList = impErrorList.tail
        }
      }
      else {
        if (!error.code.contains("-in-input-stream")) {
          isEqual = false
        }
      }
    }
    if (impErrorList != Nil) {
      txtOut4.println("notnil" + impErrorList)
      isEqual = false
    }
    isEqual
  }
}
