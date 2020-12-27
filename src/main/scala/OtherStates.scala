import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

import CharacterMatching.{isASCIIWhitespace, isControl, isNonCharacter}
import CommandStructure._
import Implement.interpretCommand
import ParseHtml.{haveReadTrans, htmlOut, readHtml, stateName}
import StateProcessedStructure.pState
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Node}

object OtherStates {
  var namedCharacterReferenceMap: Map[String, Int] = Map()
  var state80table: Map[Int, Int] = Map()

  val  Markup_declaration_open_state =
    pState("Markup_declaration_open_state",
      List(If(UNDEF("Match(next characters, IString(--))"), List(Consume("--"), Create(NewCommentToken, ""), Switch(StateName("Comment_start_state"))), List(
            If(UNDEF("DOCTYPE"), List(Consume("DOCTYPE"), Switch(StateName("DOCTYPE_state"))), List(
              If(UNDEF("[CDATA["), List(Consume("[CDATA["),
                      If(And(IsExist(""), Not(IsEqual(Non(""), Non("")))), List(Switch(StateName("CDATA_section_state"))), List(Error("cdata_in_html_content parse error"), Create(NewCommentToken, "x_1"), Set(IValueOf(IVariable("x_1")), CString("[CDATA[")), Switch(StateName("Bogus_comment_state"))))),
                List(Error("incorrectly_opened_comment parse error"), Create(NewCommentToken, ""), Switch(StateName("Bogus_comment_state")))
            ))
      )))
      ),
      List())

  val  Named_character_reference_state =
    pState("Named_character_reference_state",
      List(If(UNDEF("Match"), List(Consume("CurrentInputCharacter"), AppendTo(CurrentInputCharacter, ITemporaryBuffer),
          If(And(And(CharacterReferenceConsumedAsAttributeVal(), Not(IsEqual(CurrentInputCharacter, CChar(';')))), Or(IsEqual(NextInputCharacter, CChar('=')), IsEqual(NextInputCharacter, CChar(';')))), List(FlushCodePoint(), Switch(ReturnState)),
            List(If(UNDEF(""), List(Error("missing_semicolon_after_character_reference parse error")), List()), Set(ITemporaryBuffer, CString("")), AppendTo(CurrentInputCharacter, ITemporaryBuffer), FlushCodePoint(), Switch(ReturnState)))),
        List(FlushCodePoint(), Switch(StateName("ambiguous_ampersand_state"))))),
      List()
    )

  val  Numeric_character_reference_end_state =
    pState("Numeric_character_reference_end_state",
      List(If(IsEqual(CharacterReferenceCode, CInt(0x00)), List(Error("null_character_reference parse error"), Set(ICharacterReferenceCode, CInt(0xfffd))),
        List()),
        Set(ITemporaryBuffer, CString("")), AppendTo(CharacterReferenceCode, ITemporaryBuffer), FlushCodePoint(), Switch(ReturnState)),
      List()
    )

  def markupDeclarationOpenState(env: Environment.Env): Environment.Env = {
    var newEnv = env
    var comlist: List[Command] = List()
    newEnv.inputText match {
      case i if i.startsWith("--") => comlist ++= List(Consume("--"), Create(NewCommentToken, ""), Switch(StateName("Comment_start_state")))
      case i if i.startsWith("DOCTYPE") => comlist ++= List(Consume("DOCTYPE"), Switch(StateName("DOCTYPE_state")))
      case i if i.startsWith("[CDATA[") => { /** 条件分岐省略*/
        comlist ++= List(Consume("[CDATA["),
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
      case i if (i == 0x0D || (isControl(i) && !isASCIIWhitespace(i))) => comlist ++= List(Error("control_character_reference parse error"))
      case i => {
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

  // Numeric_character_reference_end_stateの表を取り出す
  def loadCodePointTable() = {
    // htmlのparse
    val doc: Document = Jsoup.parse(new File("src/state80table.txt"),null)

    val rootNode: Node = doc.body()
    var node = rootNode.childNode(0)

    var i = 0
    var m: Int = -1
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
      while (it.hasNext) {
        val us = it.next()
        us.toString()
      }
      namedCharacterReferenceMap += (str -> 1)
      node = node.nextSibling()
    }

    println(node)
    println("a : " + node.childNode(0).childNode(1).childNode(0).toString)
    println("c : " + node.childNode(0).nextSibling().childNode(0).toString)
  }
}
