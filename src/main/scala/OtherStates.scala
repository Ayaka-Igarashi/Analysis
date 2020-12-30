import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

import CharacterMatching.{isASCIIAlphaNumeric, isASCIIWhitespace, isControl, isNonCharacter}
import CommandStructure._
import Environment.StringVal
import Implement.{implementBool, interpretCommand}
import ParseHtml.{haveReadTrans, htmlOut, readHtml, stateName}
import StateProcessedStructure.pState
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Node}

import scala.collection.immutable.ListMap

object OtherStates {
  var namedCharacterReferenceMap: ListMap[String, String] = ListMap()
  var state80table: Map[Long, Int] = Map()

//  val  Markup_declaration_open_state =
//    pState("Markup_declaration_open_state",
//      List(If(UNDEF("Match(next characters, IString(--))"), List(Consume("--"), Create(NewCommentToken, ""), Switch(StateName("Comment_start_state"))), List(
//            If(UNDEF("DOCTYPE"), List(Consume("DOCTYPE"), Switch(StateName("DOCTYPE_state"))), List(
//              If(UNDEF("[CDATA["), List(Consume("[CDATA["),
//                      If(And(IsExist(""), Not(IsEqual(Non(""), Non("")))), List(Switch(StateName("CDATA_section_state"))), List(Error("cdata_in_html_content parse error"), Create(NewCommentToken, "x_1"), Set(IValueOf(IVariable("x_1")), CString("[CDATA[")), Switch(StateName("Bogus_comment_state"))))),
//                List(Error("incorrectly_opened_comment parse error"), Create(NewCommentToken, ""), Switch(StateName("Bogus_comment_state")))
//            ))
//      )))
//      ),
//      List())

//  val  Named_character_reference_state =
//    pState("Named_character_reference_state",
//      List(If(UNDEF("Match"), List(Consume("CurrentInputCharacter"), AppendTo(CurrentInputCharacter, ITemporaryBuffer),
//          If(And(And(CharacterReferenceConsumedAsAttributeVal(), Not(IsEqual(CurrentInputCharacter, CChar(';')))), Or(IsEqual(NextInputCharacter, CChar('=')), IsEqual(NextInputCharacter, CChar(';')))), List(FlushCodePoint(), Switch(ReturnState)),
//            List(If(UNDEF(""), List(Error("missing_semicolon_after_character_reference parse error")), List()), Set(ITemporaryBuffer, CString("")), AppendTo(CurrentInputCharacter, ITemporaryBuffer), FlushCodePoint(), Switch(ReturnState)))),
//        List(FlushCodePoint(), Switch(StateName("ambiguous_ampersand_state"))))),
//      List()
//    )
//
//  val  Numeric_character_reference_end_state =
//    pState("Numeric_character_reference_end_state",
//      List(If(IsEqual(CharacterReferenceCode, CInt(0x00)), List(Error("null_character_reference parse error"), Set(ICharacterReferenceCode, CInt(0xfffd))),
//        List()),
//        Set(ITemporaryBuffer, CString("")), AppendTo(CharacterReferenceCode, ITemporaryBuffer), FlushCodePoint(), Switch(ReturnState)),
//      List()
//    )

  def markupDeclarationOpenState(env: Environment.Env): Environment.Env = {
    var newEnv = env
    var comlist: List[Command] = List()
    newEnv.inputText match {
      case i if i.startsWith("--") => comlist ++= List(Consume(CString("--")), Create(NewCommentToken, ""), Switch(StateName("Comment_start_state")))
      case i if i.toLowerCase.startsWith("DOCTYPE".toLowerCase) => comlist ++= List(Consume(CString(i.substring(0, 7))), Switch(StateName("DOCTYPE_state")))
      case i if i.startsWith("[CDATA[") => { /** 条件分岐省略してる*/
        comlist ++= List(Consume(CString("[CDATA[")),
                      If(T, List(Switch(StateName("CDATA_section_state"))),
                            List(Error("cdata_in_html_content parse error"), Create(NewCommentToken, "x_1"), Set(IValueOf(IVariable("x_1")), CString("[CDATA[")), Switch(StateName("Bogus_comment_state")))))
      }
      case _ => comlist ++= List(Error("incorrectly_opened_comment parse error"), Create(NewCommentToken, ""), Switch(StateName("Bogus_comment_state")))
    }

    // Commandを1つずつ処理する
    for (command <- comlist) {
      newEnv = interpretCommand(newEnv, command)
    }
    newEnv
  }

  def namedCharacterReferenceState(env: Environment.Env): Environment.Env = {
    var newEnv = env
    var comlist: List[Command] = List()

    var isMatched = false
    var characterReferencePair: (String, String) = null
    for (m <- namedCharacterReferenceMap) {
      if (env.inputText.startsWith(m._1)) {
        if (!isMatched) isMatched = true
        if (characterReferencePair == null) characterReferencePair = (m._1, m._2)
        else if (characterReferencePair._1.length < m._1.length) characterReferencePair = (m._1, m._2)
      }
    }

    if (isMatched) {
      newEnv = interpretCommand(newEnv, Consume(CString(characterReferencePair._1)))
      newEnv = interpretCommand(newEnv, AppendTo(CString(characterReferencePair._1), ITemporaryBuffer))

      val b1 = implementBool(newEnv, CharacterReferenceConsumedAsAttributeVal())
      val b2 = newEnv.currentInputCharacter match {
        case StringVal(string) => string.last == ';'
        case _ => false
      }
      val b3 = newEnv.inputText.headOption == Some('=')
      val b4 = if (newEnv.inputText.headOption == None) false else isASCIIAlphaNumeric(newEnv.inputText.head)

      val b = b1 && !b2 && (b3 || b4)

      if (b) {
        comlist ++= List(FlushCodePoint(), Switch(ReturnState))
      }
      else {
        if (b2) {
          comlist :+= Error("missing_semicolon_after_character_reference parse error")
        }
        comlist ++= List(Set(ITemporaryBuffer, CString("")), AppendTo(CString(characterReferencePair._2), ITemporaryBuffer),
                          FlushCodePoint(), Switch(ReturnState))
      }
    } else {
      comlist ++= List(FlushCodePoint(), Switch(StateName("Ambiguous_ampersand_state")))
    }

    // Commandを1つずつ処理する
    for (command <- comlist) {
      newEnv = interpretCommand(newEnv, command)
    }
    newEnv
  }

  def numericCharacterReferenceEndState(env: Environment.Env): Environment.Env = {
    var newEnv = env
    var comlist: List[Command] = List()
    newEnv.characterReferenceCode match {
      case 0x00 => comlist ++= List(Error("null_character_reference parse error"), Set(ICharacterReferenceCode, CInt(0xFFFD)))
      case i if i > 0x10FFFF => comlist ++= List(Error("character_reference_outside_unicode_range parse error"), Set(ICharacterReferenceCode, CInt(0xFFFD)))
      case i if (0xD800 <= i && i <= 0xDFFF) => comlist ++= List(Error("surrogate_character_reference parse error"), Set(ICharacterReferenceCode, CInt(0xFFFD)))
      case i if isNonCharacter(i) => comlist ++= List(Error("noncharacter_character_reference parse error"))
      case i => {
        if (i == 0x0D || (isControl(i) && !isASCIIWhitespace(i))) comlist ++= List(Error("control_character_reference parse error"))
        state80table.get(i) match {
          case Some(num) => comlist :+= Set(ICharacterReferenceCode, CInt(num))
          case None =>
        }
      }
    }

    comlist ++= List(Set(ITemporaryBuffer, CString("")), AppendTo(CharacterReferenceCode, ITemporaryBuffer), FlushCodePoint(), Switch(ReturnState))

    // Commandを1つずつ処理する
    for (command <- comlist) {
      newEnv = interpretCommand(newEnv, command)
    }
    newEnv
  }

  var isLoaded = false
  def loadTable() = {
    if (!isLoaded) {
      loadCodePointTable()
      loadNamedCharacterReferenceCodeTable()
      isLoaded = true
    }
  }

  // Numeric_character_reference_end_stateの表を取り出す
  def loadCodePointTable() = {
    // htmlのparse
    val doc: Document = Jsoup.parse(new File("src/state80table.txt"),null)

    val rootNode: Node = doc.body()
    var node = rootNode.childNode(0)

    var i = 0
    var m: Long = -1
    while (node != null) {
      i match {
        case i2 if i2 % 3 == 0 => {
          val str = node.toString.replace(" ", "").replace("\n", "")
          m = Integer.parseInt(str.slice(2, str.length), 16)
        }
        case i2 if i2 % 3 == 1 => {
          val str = node.toString.replace(" ", "").replace("\n", "")
          state80table += (m -> Integer.parseInt(str.slice(2, str.length), 16))
        }
        case _ =>
      }
      node = node.nextSibling()
      i += 1
    }
  }
  // NamedCharacterReferenceCodeの表を取り出す
  def loadNamedCharacterReferenceCodeTable() = {
    // htmlのparse
    val doc: Document = Jsoup.parse(new File("src/namedCharacterReferencesCode.txt"),null)

    val rootNode: Node = doc.body()
    var node = rootNode.childNode(0).childNode(0).childNode(0)

    while (node != null) {
      val str = node.childNode(0).childNode(1).childNode(0).toString.replace(" ", "")
      val unicode_str = node.childNode(0).nextSibling().childNode(0).toString.replace(" ", "")
      val re = "U\\+[0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F]?".r
      val it = re.findAllMatchIn(unicode_str)
      var characters: String = ""
      while (it.hasNext) {
        val us = it.next()
        val s = us.toString().slice(2, us.toString().length)
        val hex = Integer.parseInt(s, 16)
        if (s.startsWith("0")) {
          characters += hex.toChar.toString
        }
        else {
          characters += (((hex - 0x10000) / 0x400 + 0xD800).toChar.toString + ((hex - 0x10000) % 0x400 + 0xDC00).toChar.toString)
        }
      }
      namedCharacterReferenceMap += (str -> characters)
      node = node.nextSibling()
    }
  }
}
