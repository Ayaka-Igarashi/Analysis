import CommandStructure._
import Environment._
import Main.{txtOut2, txtOut3}
import StateProcessedStructure.pState

import scala.collection.immutable.ListMap

object Implement {
  //env updated ("currentState", env("nextState"))

  var anythingElseCommand: List[Command] = List()
  var uniqueId: Int = 1
  // インタープリタ
  def interpret(env: Env, definition: ListMap[String, pState]): Env = {
    var newEnv: Env = env
    uniqueId = (uniqueId+ 1)
    // 最初の処理
    newEnv.currentState = newEnv.nextState
    val currentState = newEnv.currentState match {
      case StateVal(s) => s
    }
    //newEnv.emitTokens = List()
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
        // Anything elseの処理とってくる
        val lastCommand = trans.last
        if (lastCommand._1 == "Anything else") {
          anythingElseCommand = lastCommand._2
        }

        // Commandを1つずつ処理する
        for (command <- commandList) {
          newEnv = interpretCommand(newEnv, command)
        }
        txtOut3.println("command : "+ prev + " , " + commandList)
      }
      case None => println("undefined state error : " + currentState)
    }
    anythingElseCommand = List()
    newEnv
  }

  def characterMatching(currentInputCharacter: InputCharacter, trans: List[(String, List[Command])]): List[Command] = {
    trans match {
      case (character, comList) :: rst => {
        character match {
          case "ASCII upper alpha" => {
            currentInputCharacter match {
              case CharInput(c) => {
                if (c >= 0x0041 && c <= 0x005A) comList
                else characterMatching(currentInputCharacter, rst)
              }
              case _ => characterMatching(currentInputCharacter, rst)
            }
          }
          case "ASCII lower alpha" => {
            currentInputCharacter match {
              case CharInput(c) => {
                if (c >= 0x0061 && c <= 0x007A) comList
                else characterMatching(currentInputCharacter, rst)
              }
              case _ => characterMatching(currentInputCharacter, rst)
            }
          }
          case "ASCII alpha" => {
            currentInputCharacter match {
              case CharInput(c) => {
                if ((c >= 0x0041 && c <= 0x005A)||(c >= 0x0061 && c <= 0x007A)) comList
                else characterMatching(currentInputCharacter, rst)
              }
              case _ => characterMatching(currentInputCharacter, rst)
            }
          }
          case "ASCII alphanumeric" => {
            currentInputCharacter match {
              case CharInput(c) => {
                if ((c >= 0x0030 && c <= 0x0039)||(c >= 0x0041 && c <= 0x005A)||(c >= 0x0061 && c <= 0x007A)) comList
                else characterMatching(currentInputCharacter, rst)
              }
              case _ => characterMatching(currentInputCharacter, rst)
            }
          }
          case "ASCII hex digit" => {
            currentInputCharacter match {
              case CharInput(c) => {
                if ((c >= 0x0030 && c <= 0x0039)||(c >= 0x0041 && c <= 0x0046)||(c >= 0x0061 && c <= 0x0066)) comList
                else characterMatching(currentInputCharacter, rst)
              }
              case _ => characterMatching(currentInputCharacter, rst)
            }
          }
          case "ASCII upper hex digit" => {
            currentInputCharacter match {
              case CharInput(c) => {
                if ((c >= 0x0030 && c <= 0x0039)||(c >= 0x0041 && c <= 0x0046)) comList
                else characterMatching(currentInputCharacter, rst)
              }
              case _ => characterMatching(currentInputCharacter, rst)
            }
          }
          case "ASCII lower hex digit" => {
            currentInputCharacter match {
              case CharInput(c) => {
                if ((c >= 0x0030 && c <= 0x0039)||(c >= 0x0061 && c <= 0x0066)) comList
                else characterMatching(currentInputCharacter, rst)
              }
              case _ => characterMatching(currentInputCharacter, rst)
            }
          }
          case "ASCII digit" => {
            currentInputCharacter match {
              case CharInput(c) => {
                if ((c >= 0x0030 && c <= 0x0039)) comList
                else characterMatching(currentInputCharacter, rst)
              }
              case _ => characterMatching(currentInputCharacter, rst)
            }
          }
          case "Anything else" => comList
          case "EOF" => {
            if (currentInputCharacter == EOF) comList
            else characterMatching(currentInputCharacter, rst)
          }
          case s => {
            val re =  "(U\\+[0-9A-F][0-9A-F][0-9A-F][0-9A-F])".r
            re.findFirstIn(s) match {
              case Some(unicode) => {
                currentInputCharacter match {
                  case CharInput(c) => {
                    if (c == Utility.unicodeToInt(unicode)) comList
                    else characterMatching(currentInputCharacter, rst)
                  }
                  case _ => characterMatching(currentInputCharacter, rst)
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
        implementValueToValue(state, newEnv) match {
          case s: StateVal => newEnv.nextState = s
          case _ =>
        }
      }
      case Reconsume(state) => {
        implementValueToValue(state, newEnv) match {
          case s: StateVal => newEnv.nextState = s
          case _ =>
        }
        newEnv.currentInputCharacter match {
          case CharInput(c) => newEnv.inputText = c + newEnv.inputText
          case StrInput(string) => newEnv.inputText = string + newEnv.inputText
          case EOF =>
        }
        //newEnv.currentInputCharacter = null
      }
      case Set(obj, iValue) => {
        val value = implementValueToValue(iValue, newEnv)
        val value_string = value match {
          case StringVal(string) => string
          case CharVal(c) => c.toString
          case _ => ""
        }
        obj match {
          case ReturnState => {
            value match {
              case s: StateVal => newEnv.returnState = s
              case _ =>
            }
          }
          case CharacterReferenceCode => newEnv.characterReferenceCode = value match {
            case IntVal(i) => i
            case _ => println("CharacterReferenceCode error");-1000
          }
          case TemporaryBuffer => newEnv.temporaryBuffer = value_string
          case NameOf(token) => {
            var key: String = null
            token match {
              case Variable(v) => key = v + uniqueId.toString
              case CurrentTagToken => key = newEnv.currentTagToken
              case CurrentDOCTYPEToken => key = newEnv.currentDOCTYPEToken
              case CurrentAttribute => key = newEnv.currentAttribute
              case _ =>
            }
            newEnv.map.get(key) match {
              case Some(TokenVal(tagToken_(b,_,f,a))) => newEnv.addMap(key, TokenVal(tagToken_(b,value_string,f,a)))
              case Some(TokenVal(DOCTYPEToken(_,f1,f2,a))) => newEnv.addMap(key, TokenVal(DOCTYPEToken(value_string,f1,f2,a)))
              case Some(AttributeVal(Attribute(n, v))) => newEnv.addMap(key, AttributeVal(Attribute(value_string, v)))
              case _ => println("")
            }
          }
          case ValueOf(token) => {
            var key: String = null
            token match {
              case Variable(v) => key = v + uniqueId.toString
              case CurrentTagToken => key = newEnv.currentTagToken
              case CurrentDOCTYPEToken => key = newEnv.currentDOCTYPEToken
              case CurrentAttribute => key = newEnv.currentAttribute
              case _ =>
            }
            newEnv.map.get(key) match {
              case Some(AttributeVal(Attribute(n, _))) => newEnv.addMap(key, AttributeVal(Attribute(n, value_string)))
              case _ => println("")
            }
          }
          case _ =>
        }
      }
      case Consume(character) => {
        character match {
          case "next input character" => {
            if (newEnv.inputText.length > 0) {
              newEnv.currentInputCharacter = CharInput(newEnv.inputText.head)
              newEnv.inputText = newEnv.inputText.tail
            } else {
              newEnv.currentInputCharacter = EOF
            }
          }
          case _ => {
            // 途中
            if (newEnv.inputText.substring(0, character.length) == character) {
              newEnv.inputText = newEnv.inputText.substring(character.length - 1)
              newEnv.currentInputCharacter = StrInput(character)
            } else {
              println("consume error")
            }
          }
        }
      }
      case Emit(token) => {
        val value = implementValueToValue(token, newEnv)
        val t = ValueToToken(value, newEnv)
        t match {
          case tagToken(true, name, _, _) => newEnv.lastStartTagName = name
          case _ =>
        }
        if (t != null) newEnv.addEmitToken(t)
        else {
          newEnv.addEmitToken(characterToken(null))
          txtOut3.println("emit error" + value)
        }
      }
      case Append(iValue, obj) => {
        val value = implementValueToValue(iValue, newEnv)
        val appendStr: String = value match {
          case StringVal(string) => string
          case CharVal(c) => c.toString
          case _ => ""
        }
        obj match {
          case CommentToken => {newEnv.map.get(newEnv.commentToken) match {
            case Some(TokenVal(Environment.commentToken(s))) => newEnv.addMap(newEnv.commentToken, TokenVal(Environment.commentToken(s + appendStr)))
            case _ =>
          }}
          case TemporaryBuffer => newEnv.temporaryBuffer += appendStr
          case NameOf(token) => {
            var key: String = null
            token match {
              case Variable(v) => key = v + uniqueId.toString
              case CurrentTagToken => key = newEnv.currentTagToken
              case CurrentDOCTYPEToken => key = newEnv.currentDOCTYPEToken
              case CurrentAttribute => key = newEnv.currentAttribute
              case _ =>
            }
            newEnv.map.get(key) match {
              case Some(TokenVal(tagToken_(b,n,f,a))) => newEnv.addMap(key, TokenVal(tagToken_(b,n + appendStr,f,a)))
              case Some(TokenVal(DOCTYPEToken(n,f1,f2,a))) => newEnv.addMap(key, TokenVal(DOCTYPEToken(n + appendStr,f1,f2,a)))
              case Some(AttributeVal(Attribute(n,v))) => newEnv.addMap(key, AttributeVal(Attribute(n + appendStr, v)))
              case _ => println("")
            }
          }
          case ValueOf(token) => {
            var key: String = null
            token match {
              case Variable(v) => key = v + uniqueId.toString
              case CurrentTagToken => key = newEnv.currentTagToken
              case CurrentDOCTYPEToken => key = newEnv.currentDOCTYPEToken
              case CurrentAttribute => key = newEnv.currentAttribute
              case _ =>
            }
            newEnv.map.get(key) match {
              case Some(AttributeVal(Attribute(n, v))) => newEnv.addMap(key, AttributeVal(Attribute(n, appendStr + v)))
              case _ => println("")
            }
          }
          case _ =>
        }
      }
      case Error(error) => newEnv.errorContent = error //println("ErrorCode : "+error)
      case Create(iVal, corefKey) => {
        val key = if (corefKey == "") "token_" + newEnv.getID() else corefKey + uniqueId.toString
        newEnv.addMap(key, implementValueToValue(iVal, newEnv))
        iVal match {
          case NewEndTagToken | NewStartTagToken => newEnv.currentTagToken = key
          case NewDOCTYPEToken => newEnv.currentDOCTYPEToken = key
          case NewCommentToken => newEnv.commentToken = key
          case _ => println("error")
        }
      }
      case Ignore(_) => //println("ignore") // 何もしない
      case FlushCodePoint() => {
        val flushCommand =  If(CharacterReferenceConsumedAsAttributeVal(),
                              List(Append(TemporaryBuffer, ValueOf(Variable(newEnv.currentAttribute)))),
                               List(Emit(TemporaryBuffer)))
        newEnv = interpretCommand(newEnv, flushCommand)
      }
      case TreatAsAnythingElse() => {
        // AnithingElseのコマンド実行する
        val comList = anythingElseCommand
        for (c <- comList) newEnv = interpretCommand(newEnv, c)
      }
      case StartNewAttribute(corefId) => {
        // currentTagTokenに新しい属性を追加する
        val key = if (corefId == "x_-1") "tag_token_attribute_" + newEnv.getID() else corefId + uniqueId
        val newAttribute = Attribute(null, null)
        newEnv.map.get(newEnv.currentTagToken) match {
          case Some(TokenVal(tagToken_(b, s, f, attributes))) => {
            val newTagToken = tagToken_(b, s, f, attributes :+ key)
            newEnv.addMap(newEnv.currentTagToken, TokenVal(newTagToken))
          }
          case None => println("cant find current tag token")
        }
        newEnv.addMap(key, AttributeVal(newAttribute))
        newEnv.currentAttribute = key
      }
      case MultiplyBy(obj, by) => {
        val value = implementValueToValue(by, newEnv)
        val num = ValueToInt(value, newEnv)
        obj match {
          case CharacterReferenceCode => newEnv.characterReferenceCode *= num
          case _ => println("multiply error")
        }
      }
      case AddTo(obj, to) => {
        val value = implementValueToValue(obj, newEnv)
        val num = ValueToInt(value, newEnv)
        to match {
          case ICharacterReferenceCode => newEnv.characterReferenceCode += num
          case _ => println("add error")
        }
      }
      case If(bool, t, f) => {
        var comList: List[Command] = null
        if (implementBool(newEnv, bool)) comList = t else comList = f
        for (c <- comList) newEnv = interpretCommand(newEnv, c)
      }
      case IF_(_) | OTHERWISE_() => println("IF not converted error : " + command)
      case _ => println("undefined command error : " + command)
    }
    newEnv
  }

  def implementBool(env: Env, bool: Bool): Boolean = {
    bool match {
      case And(b1, b2) => implementBool(env, b1) && implementBool(env, b2)
      case Or(b1, b2) => implementBool(env, b1) || implementBool(env, b2)
      case Not(b) => !implementBool(env, b)
      case CharacterReferenceConsumedAsAttributeVal() => {
        env.returnState match {
          case StateVal("Attribute_value_double_quoted_state") | StateVal("Attribute_value_single_quoted_state")
               | StateVal("Attribute_value_unquoted_state") => true
          case _ => false
        }
      }
      case CurrentEndTagIsAppropriate() => {
        val endTagName = env.map.get(env.currentTagToken) match {
          case Some(TokenVal(tagToken_(false, name, _, _))) => name
          case _ => println("cant find current end tag token");null
        }
        if (env.lastStartTagName != null && endTagName == env.lastStartTagName) true
        else false
      }
        // 途中
      case IsEqual(a, b) => implementValueToValue(a, env) == implementValueToValue(b, env)
      case IsExist(a) => false //
      case UNDEF(str) => println("undefined bool : " + bool);false
      case _ => println("undefined bool error : " + bool);false
    }
  }

  // ImplementValueからValueに変換する
  def implementValueToValue(implementVal: CommandValue, env: Env): Value = {
    var value: Value = null
    implementVal match {
      case CChar(c) => value = CharVal(c)
      case CString(s) => value = StringVal(s)
      case CInt(i) => value = IntVal(i)
      case ReturnState => value = env.returnState
      case StateName(s) => value = StateVal(s)
      case TemporaryBuffer => value = StringVal(env.temporaryBuffer)
      case NewStartTagToken => value = TokenVal(Environment.tagToken_(true, null, false, List()))
      case NewEndTagToken => value = TokenVal(Environment.tagToken_(false, null, false, List()))
      case NewDOCTYPEToken => value = TokenVal(Environment.DOCTYPEToken(null, null, null, false))
      case NewCommentToken => value = TokenVal(Environment.commentToken(""))
      case LowerCase(i) => implementValueToValue(i, env) match {
        case CharVal(c) => value = CharVal((c + 0x20).toChar)
        case _ =>
      }
      case NumericVersion(i) => implementValueToValue(i, env) match {
        case CharVal(c) => value = CharVal((c - 0x30).toChar)
        case _ =>
      }
      case CurrentInputCharacter => env.currentInputCharacter match {
        case CharInput(c) => value = CharVal(c)
        case StrInput(s) => value = StringVal(s)
        case EOF => value = EOFVal
        case _ =>
      }
      case CurrentTagToken => {
        env.map.get(env.currentTagToken) match {
          case Some(t) => value = t
          case None => println("cant find current tag token")
        }
      }
      case CommandStructure.CurrentDOCTYPEToken => {
        env.map.get(env.currentDOCTYPEToken) match {
          case Some(TokenVal(t)) => value = TokenVal(t)
          case None => println("cant find DOCTYPE token")
        }
      }
      case CommandStructure.CommentToken => {
        env.map.get(env.commentToken) match {
          case Some(TokenVal(t)) => value = TokenVal(t)
          case None => println("cant find comment token")
        }
      }
      case CommandStructure.EndOfFileToken => value = TokenVal(endOfFileToken())
      case CommandStructure.CharacterToken(c) => value = TokenVal(characterToken(c))
      case NameOf(x) => {

      }
      case Variable(x) => {
        env.map.get(x + uniqueId.toString) match {
          case Some(a) => value = a
          case None => println("cant find : " + x + " in map")
        }
      }
      case _ =>
    }
    value
  }

  // ValueからIntに変換する
  def ValueToInt(tVal: Value, env: Env): Int = {
    tVal match {
      case CharVal(char) => char
      case IntVal(i) => i
      case _ => 0
    }
  }

  // ValueからTokenに変換する
  def ValueToToken(tVal: Value, env: Env): Token = {
    tVal match {
      case TokenVal(t) => {
        t match {
          case tagToken_(b,n,f,list) => {
            val l = getAttributeFromKey(env, list)
            tagToken(b,n,f,l)
          }
          case _ => t
        }
      }
      case StringVal(s) => characterToken(s)
      case CharVal(c) => characterToken(c.toString)
      case _ => null
    }
  }

  def getAttributeFromKey(env: Env, attributeList: List[String]): List[Attribute] = {
    attributeList.map(key => {
      env.map.get(key) match {
        case Some(AttributeVal(attribute)) => attribute
        case _ => {println("cant find attribute");Attribute(null,null)}
      }
    })
  }
}
