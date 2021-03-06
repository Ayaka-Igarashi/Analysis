import CommandStructure._
import Environment._
import Main.{txtOut2, txtOut3}
import StateProcessedStructure.pState

import scala.collection.convert.ImplicitConversions.`collection asJava`
import scala.collection.immutable.ListMap

object Implement {
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
    //newEnv.errorContent = null

    currentState match {
      case "Markup_declaration_open_state"|"Markup declaration open state" => newEnv = OtherStates.markupDeclarationOpenState(newEnv)
      case "Named_character_reference_state"|"Named character reference state" => {
        OtherStates.loadTable()
        newEnv = OtherStates.namedCharacterReferenceState(newEnv)
      }
      case "Numeric_character_reference_end_state"|"Numeric character reference end state" => {
        OtherStates.loadTable()
        newEnv = OtherStates.numericCharacterReferenceEndState(newEnv)
      }
      case _ => {
        // 状態のマッチ
        definition.get(currentState) match {
          case Some(pState(_, prev, trans)) => {
            for (command <- prev) {
              newEnv = interpretCommand(newEnv, command)
            }

            var commandList: List[Command] = List()
            if (newEnv.currentInputCharacter != null) {
              // マッチング
              commandList = characterMatching(newEnv.currentInputCharacter, trans)

              // Anything elseの処理とってくる
              val lastCommand = trans.last
              if (lastCommand._1 == "Anything else") {
                anythingElseCommand = lastCommand._2
              }
            }

            // Commandを1つずつ処理する
            for (command <- commandList) {
              newEnv = interpretCommand(newEnv, command)
            }

            // attribute name stateの後処理
            if (currentState == "Attribute_name_state" && newEnv.nextState != StateVal("Attribute_name_state")) {
              val name = newEnv.map.get(newEnv.currentAttribute) match {
                case Some(AttributeVal(Attribute(n,_))) => n
                case _ => null
              }
              newEnv.map.get(newEnv.currentTagToken) match {
                case Some(TokenVal(tagToken_(s, n, f, list))) => {
                  var remove = false
                  for (a <- list) {
                    if (a != newEnv.currentAttribute) {
                      newEnv.map.get(a) match {
                        case Some(AttributeVal(Attribute(n,_))) => {if (n == name) remove = true }
                        case _ =>
                      }
                    }
                  }
                  if (remove) {
                    newEnv.errorContent :+= "duplicate_attribute parse error"
                    newEnv.addMap(newEnv.currentTagToken, TokenVal(tagToken_(s, n, f, list.slice(0, list.length -1))))
                  }
                }
                case _ =>
              }
            }

            txtOut3.println("command : "+ prev + " , " + commandList)
          }
          case None => println("undefined state error : " + currentState)
        }
      }
    }
    anythingElseCommand = List()
    newEnv
  }

  def characterMatching(currentInputCharacter: Value, trans: List[(String, List[Command])]): List[Command] = {
    trans match {
      case (character, comList) :: rst => {
        character match {
          case "ASCII upper alpha" => {
            currentInputCharacter match {
              case CharVal(c) => {
                if (c >= 0x0041 && c <= 0x005A) comList
                else characterMatching(currentInputCharacter, rst)
              }
              case _ => characterMatching(currentInputCharacter, rst)
            }
          }
          case "ASCII lower alpha" => {
            currentInputCharacter match {
              case CharVal(c) => {
                if (c >= 0x0061 && c <= 0x007A) comList
                else characterMatching(currentInputCharacter, rst)
              }
              case _ => characterMatching(currentInputCharacter, rst)
            }
          }
          case "ASCII alpha" => {
            currentInputCharacter match {
              case CharVal(c) => {
                if ((c >= 0x0041 && c <= 0x005A)||(c >= 0x0061 && c <= 0x007A)) comList
                else characterMatching(currentInputCharacter, rst)
              }
              case _ => characterMatching(currentInputCharacter, rst)
            }
          }
          case "ASCII alphanumeric" => {
            currentInputCharacter match {
              case CharVal(c) => {
                if ((c >= 0x0030 && c <= 0x0039)||(c >= 0x0041 && c <= 0x005A)||(c >= 0x0061 && c <= 0x007A)) comList
                else characterMatching(currentInputCharacter, rst)
              }
              case _ => characterMatching(currentInputCharacter, rst)
            }
          }
          case "ASCII hex digit" => {
            currentInputCharacter match {
              case CharVal(c) => {
                if ((c >= 0x0030 && c <= 0x0039)||(c >= 0x0041 && c <= 0x0046)||(c >= 0x0061 && c <= 0x0066)) comList
                else characterMatching(currentInputCharacter, rst)
              }
              case _ => characterMatching(currentInputCharacter, rst)
            }
          }
          case "ASCII upper hex digit" => {
            currentInputCharacter match {
              case CharVal(c) => {
                if ((c >= 0x0030 && c <= 0x0039)||(c >= 0x0041 && c <= 0x0046)) comList
                else characterMatching(currentInputCharacter, rst)
              }
              case _ => characterMatching(currentInputCharacter, rst)
            }
          }
          case "ASCII lower hex digit" => {
            currentInputCharacter match {
              case CharVal(c) => {
                if ((c >= 0x0030 && c <= 0x0039)||(c >= 0x0061 && c <= 0x0066)) comList
                else characterMatching(currentInputCharacter, rst)
              }
              case _ => characterMatching(currentInputCharacter, rst)
            }
          }
          case "ASCII digit" => {
            currentInputCharacter match {
              case CharVal(c) => {
                if ((c >= 0x0030 && c <= 0x0039)) comList
                else characterMatching(currentInputCharacter, rst)
              }
              case _ => characterMatching(currentInputCharacter, rst)
            }
          }
          case "Anything else" => comList
          case "EOF" => {
            if (currentInputCharacter == EOFVal) comList
            else characterMatching(currentInputCharacter, rst)
          }
          case s => {
            val re =  "(U\\+[0-9A-F][0-9A-F][0-9A-F][0-9A-F])".r
            re.findFirstIn(s) match {
              case Some(unicode) => {
                currentInputCharacter match {
                  case CharVal(c) => {
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
        val (value, e) = commandValueToValue(state, newEnv)
        newEnv = e
        value match {
          case s: StateVal => newEnv.nextState = s
          case _ =>
        }
      }
      case Reconsume(state) => {
        val (value, e) = commandValueToValue(state, newEnv)
        newEnv = e
        value match {
          case s: StateVal => newEnv.nextState = s
          case _ =>
        }
//        newEnv.inputText = Add(newEnv.currentInputCharacter, newEnv.inputText)
        newEnv.currentInputCharacter match {
          case CharVal(c) => newEnv.inputText = c + newEnv.inputText
          case StringVal(string) => newEnv.inputText = string + newEnv.inputText
          case EOFVal =>
        }
      }
      case Set(obj, iValue) => {
        val (value, e) = commandValueToValue(iValue, newEnv)
        newEnv = e
        val value_string = valueToString(value)
        val value_bool = valueToBool(value)
        obj match {
          case IReturnState => {
            value match {
              case s: StateVal => newEnv.returnState = s
              case _ =>
            }
          }
          case ICharacterReferenceCode => newEnv.characterReferenceCode = value match {
            case IntVal(i) => i
            case _ => println("CharacterReferenceCode error");-1000
          }
          case ITemporaryBuffer => newEnv.temporaryBuffer = value_string
          case INameOf(token) => {
            var key: String = null
            token match {
              case IVariable(v) => key = v + uniqueId.toString
              case ICurrentTagToken => key = newEnv.currentTagToken
              case ICurrentDOCTYPEToken => key = newEnv.currentDOCTYPEToken
              case ICurrentAttribute => key = newEnv.currentAttribute
              case _ =>
            }
            newEnv.map.get(key) match {
              case Some(TokenVal(tagToken_(b,_,f,a))) => newEnv.addMap(key, TokenVal(tagToken_(b,value_string,f,a)))
              case Some(TokenVal(DOCTYPEToken(_,f1,f2,a))) => newEnv.addMap(key, TokenVal(DOCTYPEToken(value_string,f1,f2,a)))
              case Some(AttributeVal(Attribute(n, v))) => newEnv.addMap(key, AttributeVal(Attribute(value_string, v)))
              case _ => println("")
            }
          }
          case IValueOf(token) => {
            var key: String = null
            token match {
              case IVariable(v) => key = v + uniqueId.toString
              case ICurrentTagToken => key = newEnv.currentTagToken
              case ICurrentDOCTYPEToken => key = newEnv.currentDOCTYPEToken
              case ICurrentAttribute => key = newEnv.currentAttribute
              case _ =>
            }
            newEnv.map.get(key) match {
              case Some(AttributeVal(Attribute(n, _))) => newEnv.addMap(key, AttributeVal(Attribute(n, value_string)))
              case _ => println("")
            }
          }
          case IFlagOf(token) => {
            var key: String = null
            token match {
              case IVariable(v) => key = v + uniqueId.toString
              case ICurrentTagToken => key = newEnv.currentTagToken
              case ICurrentDOCTYPEToken => key = newEnv.currentDOCTYPEToken
              case _ =>
            }
            newEnv.map.get(key) match {
              case Some(TokenVal(tagToken_(b,n,_,a))) => newEnv.addMap(key, TokenVal(tagToken_(b,n,value_bool,a)))
              case Some(TokenVal(DOCTYPEToken(n,f1,f2,_))) => newEnv.addMap(key, TokenVal(DOCTYPEToken(n,f1,f2,value_bool)))
              case _ => println("")
            }
          }
          case PublicIdentifierOf(token) => {
            var key: String = null
            token match {
              case IVariable(v) => key = v + uniqueId.toString
              case ICurrentDOCTYPEToken => key = newEnv.currentDOCTYPEToken
              case _ =>
            }
            newEnv.map.get(key) match {
              case Some(TokenVal(DOCTYPEToken(n,_,f2,b))) => newEnv.addMap(key, TokenVal(DOCTYPEToken(n,value_string,f2,b)))
              case _ => println("")
            }
          }
          case SystemIdentifierOf(token) => {
            var key: String = null
            token match {
              case IVariable(v) => key = v + uniqueId.toString
              case ICurrentDOCTYPEToken => key = newEnv.currentDOCTYPEToken
              case _ =>
            }
            newEnv.map.get(key) match {
              case Some(TokenVal(DOCTYPEToken(n,f1,_,b))) => newEnv.addMap(key, TokenVal(DOCTYPEToken(n,f1,value_string,b)))
              case _ => println("")
            }
          }
          case _ =>
        }
      }
      case Consume(character) => {
        val (value, e) = commandValueToValue(character, newEnv)
        newEnv = e
        value match {
          case CharVal(c) => {
            if (newEnv.inputText.head == c) newEnv.inputText = newEnv.inputText.tail
            else println("consume error : " + CharVal(c))
          }
          case StringVal(s) => {
            if (newEnv.inputText.startsWith(s)) newEnv.inputText = newEnv.inputText.substring(s.length)
              /** ここ本来は違う*/
            else if (newEnv.currentInputCharacter == CharVal(s.head) && newEnv.inputText.startsWith(s.tail)) newEnv.inputText = newEnv.inputText.substring(s.length - 1)
//            else println("consume error : " + StringVal(s))
          }
          case EOFVal =>
          case _ =>
        }
        newEnv.currentInputCharacter = value
      }
      case Emit(token) => {
        val (value,e) = commandValueToValue(token, newEnv)
        newEnv = e
        val t = ValueToToken(value, newEnv)
        t match {
          case tagToken(true, name, _, _) => newEnv.lastStartTagName = name
          case tagToken(false, _, flag, attributes)  => {
            if (attributes != List()) newEnv.errorContent :+= "end_tag_with_attributes parse error"
            if (flag == true) newEnv.errorContent :+= "end_tag_with_trailing_solidus parse error"
          }
          case _ =>
        }
        if (t != null) newEnv.addEmitToken(t)
        else {
          newEnv.addEmitToken(characterToken(null))
          txtOut3.println("emit error" + value)
        }
      }
      case AppendTo(iValue, obj) => {
        val (value, e) = commandValueToValue(iValue, newEnv)
        newEnv = e
        val appendStr: String = valueToString(value)

        obj match {
          case ICommentToken => {newEnv.map.get(newEnv.commentToken) match {
            case Some(TokenVal(Environment.commentToken(s))) => newEnv.addMap(newEnv.commentToken, TokenVal(Environment.commentToken(s + appendStr)))
            case _ =>
          }}
          case ITemporaryBuffer => newEnv.temporaryBuffer += appendStr
          case INameOf(token) => {
            var key: String = null
            token match {
              case IVariable(v) => key = v + uniqueId.toString
              case ICurrentTagToken => key = newEnv.currentTagToken
              case ICurrentDOCTYPEToken => key = newEnv.currentDOCTYPEToken
              case ICurrentAttribute => key = newEnv.currentAttribute
              case _ =>
            }
            newEnv.map.get(key) match {
              case Some(TokenVal(tagToken_(b,n,f,a))) => newEnv.addMap(key, TokenVal(tagToken_(b,n + appendStr,f,a)))
              case Some(TokenVal(DOCTYPEToken(n,f1,f2,a))) => newEnv.addMap(key, TokenVal(DOCTYPEToken(n + appendStr,f1,f2,a)))
              case Some(AttributeVal(Attribute(n,v))) => newEnv.addMap(key, AttributeVal(Attribute(n + appendStr, v)))
              case _ => println("")
            }
          }
          case IValueOf(token) => {
            var key: String = null
            token match {
              case IVariable(v) => key = v + uniqueId.toString
              case ICurrentTagToken => key = newEnv.currentTagToken
              case ICurrentDOCTYPEToken => key = newEnv.currentDOCTYPEToken
              case ICurrentAttribute => key = newEnv.currentAttribute
              case _ =>
            }
            newEnv.map.get(key) match {
              case Some(AttributeVal(Attribute(n, v))) => newEnv.addMap(key, AttributeVal(Attribute(n, v + appendStr)))
              case _ => println("")
            }
          }
          case PublicIdentifierOf(token) => {
            var key: String = null
            token match {
              case IVariable(v) => key = v + uniqueId.toString
              case ICurrentDOCTYPEToken => key = newEnv.currentDOCTYPEToken
              case _ =>
            }
            newEnv.map.get(key) match {
              case Some(TokenVal(DOCTYPEToken(n,f1,f2,b))) => newEnv.addMap(key, TokenVal(DOCTYPEToken(n,f1 + appendStr,f2,b)))
              case _ => println("")
            }
          }
          case SystemIdentifierOf(token) => {
            var key: String = null
            token match {
              case IVariable(v) => key = v + uniqueId.toString
              case ICurrentDOCTYPEToken => key = newEnv.currentDOCTYPEToken
              case _ =>
            }
            newEnv.map.get(key) match {
              case Some(TokenVal(DOCTYPEToken(n,f1,f2,b))) => newEnv.addMap(key, TokenVal(DOCTYPEToken(n,f1,f2 + appendStr,b)))
              case _ => println("")
            }
          }
          case _ =>
        }
      }
      case Error(error) => newEnv.errorContent :+= error //println("ErrorCode : "+error)
      case Create(iVal, corefKey) => {
        val key = if (corefKey == "") "token_" + newEnv.getID() else corefKey + uniqueId.toString
        val (value,e) = commandValueToValue(iVal, newEnv)
        newEnv = e
        newEnv.addMap(key, value)
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
                              List(AppendTo(TemporaryBuffer, IValueOf(ICurrentAttribute))),
                               List(Emit(CharacterToken(TemporaryBuffer))))
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
        val (value, e) = commandValueToValue(by, newEnv)
        newEnv = e
        val num = ValueToInt(value, newEnv)
        obj match {
          case ICharacterReferenceCode => {
            if (!(newEnv.characterReferenceCode >= Long.MaxValue / num)) newEnv.characterReferenceCode *= num
          }
          case _ => println("multiply error")
        }
      }
      case AddTo(obj, to) => {
        val (value,e) = commandValueToValue(obj, newEnv)
        newEnv = e
        val num = ValueToInt(value, newEnv)
        to match {
          case ICharacterReferenceCode => newEnv.characterReferenceCode += num
          case _ => println("add error")
        }
      }
      case If(bool, t, f) => {
        var comList: List[Command] = null
        val (b,e) = implementBool(newEnv, bool)
        newEnv = e
        if (b) comList = t else comList = f
        for (c <- comList) newEnv = interpretCommand(newEnv, c)
      }
      case IF_(_) | OTHERWISE_() => println("IF not converted error : " + command)
      case _ => //println("undefined command error : " + command)
    }
    newEnv
  }

  def implementBool(env: Env, bool: Bool): (Boolean, Env) = {
    bool match {
      case T => (true, env)
      case F => (false, env)
      case And(b1, b2) => (implementBool(env, b1)._1 && implementBool(env, b2)._1, env)
      case Or(b1, b2) => (implementBool(env, b1)._1 || implementBool(env, b2)._1, env)
      case Not(b) => (!implementBool(env, b)._1, env)
      case CharacterReferenceConsumedAsAttributeVal() => {
        env.returnState match {
          case StateVal("Attribute_value_double_quoted_state") | StateVal("Attribute_value_single_quoted_state")
               | StateVal("Attribute_value_unquoted_state") => (true, env)
          case _ => (false, env)
        }
      }
      case CurrentEndTagIsAppropriate() => {
        val endTagName = env.map.get(env.currentTagToken) match {
          case Some(TokenVal(tagToken_(false, name, _, _))) => name
          case _ => println("cant find current end tag token");null
        }
        if (env.lastStartTagName != null && endTagName == env.lastStartTagName) (true, env)
        else (false, env)
      }
      case IsEqual(a, b) => {
        val (value1,e1) = commandValueToValue(a, env)
        val (value2,e2) = commandValueToValue(b, e1)
        (value1 == value2, e2)
      }
      case AsciiCaseInsensitiveMatch(a, b) => {
        val (value1,e1) = commandValueToValue(a, env)
        val (value2,e2) = commandValueToValue(b, e1)
        (valueToString(value1).toLowerCase() == valueToString(value2).toLowerCase(), e2)
      }
      //case IsExist(a) => false //
      case UNDEF(str) => println("undefined bool : " + bool);(false,env)
      case _ => println("undefined bool error : " + bool);(false,env)
    }
  }

  // CommandValueからValueに変換する
  def commandValueToValue(commandVal: CommandValue, env: Env): (Value, Env) = {
    var newEnv: Env = env
    var value: Value = null
    commandVal match {
      case CChar(c) => value = CharVal(c)
      case CString(s) => value = StringVal(s)
      case CInt(i) => value = IntVal(i)
      case CBool(t) => value = BoolVal(t)
      case ReturnState => value = env.returnState
      case StateName(s) => value = StateVal(s)
      case TemporaryBuffer => value = StringVal(env.temporaryBuffer)
      case CharacterReferenceCode => value = IntVal(env.characterReferenceCode)
      case NewStartTagToken => value = TokenVal(Environment.tagToken_(true, null, false, List()))
      case NewEndTagToken => value = TokenVal(Environment.tagToken_(false, null, false, List()))
      case NewDOCTYPEToken => value = TokenVal(Environment.DOCTYPEToken(null, null, null, false))
      case NewCommentToken => value = TokenVal(Environment.commentToken(""))
      case LowerCase(i) => commandValueToValue(i, env)._1 match {
        case CharVal(c) => value = CharVal((c + 0x20).toChar)
        case StateVal(s) => value = StateVal(s.toLowerCase)
        case _ =>
      }
      case NumericVersion(i) => commandValueToValue(i, env)._1 match {
        case CharVal(c) => value = IntVal(Integer.parseInt(c.toString, 16))
        case StateVal(s) => value = IntVal(Integer.parseInt(s, 16))
        case _ =>
      }
      case CurrentInputCharacter => value = env.currentInputCharacter
      case NextInputCharacter(i) => {
        if (i == 1) {
          value = env.inputText.headOption match {
            case Some(c) => CharVal(c)
            case None => EOFVal
          }
        } else {
          value = StringVal(env.inputText.slice(0, i))
        }
      }
      case CharactersFromCurrentInputCharacter(i) => {
        env.currentInputCharacter match {
          case CharVal(c) => value = StringVal(c + env.inputText.slice(0, i - 1))
          case StringVal(s) => value = StringVal(s + env.inputText.slice(0, i - s.length))
          case _ => value = StringVal(env.inputText.slice(0, i))
        }
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
      case CommandStructure.CharacterToken(cval) => {
//        value = TokenVal(characterToken(c))
         commandValueToValue(cval, env)._1 match {
                case CharVal(c) => value = TokenVal(characterToken(c.toString))
                case StringVal(s) => value = TokenVal(characterToken(s))
                case IntVal(i) => value = TokenVal(characterToken(i.toChar.toString))
                case _ =>
              }
      }
      case Variable(x) => {
        env.map.get(x + uniqueId.toString) match {
          case Some(a) => value = a
          case None => //println("cant find : " + x + " in map")
        }
      }
      case Substitute(Variable(x), c) => {
        value = commandValueToValue(c ,env)._1
        newEnv.addMap(x + uniqueId.toString, value)
      }
      case _ =>
    }
    (value, newEnv)
  }

  // ValueからIntに変換する
  def ValueToInt(tVal: Value, env: Env): Long = {
    tVal match {
      case CharVal(char) => char
      case IntVal(i) => i
      case _ => 0
    }
  }

  // ValueからStringに変換する
  def valueToString(tVal: Value): String = {
    tVal match {
      case StringVal(s) => s
      case CharVal(c) => c.toString
      case IntVal(i) => Utility.longIntToCharacter(i)
      case _ => ""
    }
  }

  def valueToBool(tVal: Value): Boolean = {
    tVal match {
      case BoolVal(b) => b
      case _ => false
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
      case IntVal(i) => characterToken(i.toChar.toString)
      case _ => null
    }
  }

  def implementVariableToVariable(implementVariable: ImplementVariable, env: Env) = {
    implementVariable match {
      case IReturnState =>
      case ITemporaryBuffer =>
      case ICharacterReferenceCode =>
      case ICurrentTagToken =>
      case ICurrentDOCTYPEToken =>
      case ICurrentAttribute =>
      case ICommentToken =>
      case IVariable(x) =>
      case INameOf(i) =>
      case IValueOf(i) =>
      case IFlagOf(i) =>
      case _ =>
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
