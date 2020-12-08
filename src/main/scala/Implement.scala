import CommandStructure._
import Environment._
import Main.{txtOut2, txtOut3}
import StateProcessedStructure.pState

import scala.collection.immutable.ListMap

object Implement {
  //env updated ("currentState", env("nextState"))

  // インタープリタ
  def interpret(env: Env, definition: ListMap[String, pState]): Env = { // env(環境)も引数に入れる,返り値もenvにする
    var newEnv: Env = env
    // 最初の処理
    val currentState = newEnv.nextState
    newEnv.currentState = currentState
    newEnv.emitTokens = List()
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
        // Commandを1つずつ処理する
        for (command <- commandList) {
          newEnv = interpretCommand(newEnv, command)
        }
        txtOut3.println("command : "+ prev + " , " + commandList)
      }
      case None => println("undefined state error : " + currentState)
    }
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
        state match {
          case ReturnState => newEnv.nextState = newEnv.returnState
          case StateName(name) => newEnv.nextState = name
        }
//        if (state == ReturnState) newEnv.nextState = newEnv.returnState
//        else newEnv.nextState = state
      }
      case Reconsume(state) => {
        if (state == "return state") newEnv.nextState = newEnv.returnState
        else newEnv.nextState = state
        newEnv.currentInputCharacter match {
          case CharInput(c) => newEnv.inputText = c + newEnv.inputText
          case StrInput(string) => newEnv.inputText = string + newEnv.inputText
          case EOF =>
        }
        //newEnv.currentInputCharacter = null
      }
      case Set(obj, to) => {
        obj match {
          case ReturnState => {
            to match {
              case StateName(s) => newEnv.returnState = s
              case _ =>
            }
          }
          case TemporaryBuffer => {
            to match {
              case Mojiretu(string) => newEnv.temporaryBuffer = string
              case _ =>
            }
          }
          case NameOf(token) => {
            var name: String = null
            to match {
              case Mojiretu(string) => name = string
              case _ =>
            }
            var key: String = null
            token match {
              case Variable(v) => key = v
              case CurrentTagToken => key = newEnv.currentTagToken
              case CurrentDOCTYPEToken => key = newEnv.currentDOCTYPEToken
              case _ =>
            }
            newEnv.env.get(key) match {
              case Some(TokenVal(tagToken_(b,_,f,a))) => newEnv.addMap(key, TokenVal(tagToken_(b,name,f,a)))
              case Some(TokenVal(DOCTYPEToken(_,f1,f2,a))) => newEnv.addMap(key, TokenVal(DOCTYPEToken(name,f1,f2,a)))
              case Some(AttributeVal(Attribute(n, v))) => newEnv.addMap(key, AttributeVal(Attribute(name, v)))
              case _ => println("")
            }
          }
          case ValueOf(Variable(v)) =>
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
        token match {
          case CommandStructure.CurrentTagToken => {
            newEnv.env.get(newEnv.currentTagToken) match {
              case Some(TokenVal(tagToken_(b,n,f,list))) => {
                val l = getAttributeFromKey(newEnv, list)
                newEnv.addEmitToken(tagToken(b,n,f,l))
              }
              case None => println("cant find current tag token")
            }
          }
          case CommandStructure.CurrentDOCTYPEToken => {
            newEnv.env.get(newEnv.currentDOCTYPEToken) match {
              case Some(TokenVal(t)) => newEnv.addEmitToken(t)
              case None => println("cant find DOCTYPE token")
            }
          }
          case CommandStructure.CommentToken => {
            newEnv.env.get(newEnv.commentToken) match {
              case Some(TokenVal(t)) => newEnv.addEmitToken(t)
              case None => println("cant find comment token")
            }
          }
          case CommandStructure.EndOfFileToken => newEnv.addEmitToken(endOfFileToken())
          case CommandStructure.CharacterToken(c) => newEnv.addEmitToken(characterToken(c))
          case CommandStructure.CurrentInputCharacter => {
            newEnv.currentInputCharacter match {
              case CharInput(c) => newEnv.addEmitToken(characterToken(c.toString))
              case StrInput(string) => newEnv.addEmitToken(characterToken(string))
              case _ =>
            }
          }
          case CommandStructure.Variable(x) => {
            newEnv.env.get(x) match {
              case Some(TokenVal(tagToken_(b,n,f,list))) => {
                val l = getAttributeFromKey(newEnv, list)
                newEnv.addEmitToken(tagToken(b,n,f,l))
              }
              case Some(TokenVal(t)) => newEnv.addEmitToken(t)
              case None => println("cant find token : " + x)
            }
          }
          case _ => {
            newEnv.addEmitToken(characterToken(null))
            txtOut3.println("emit error" + token)
          }
        }
      }
      case Append(obj, to) => {
        to match {
          case TemporaryBuffer => {

          }
          case NameOf(token) => {
            var name: String = null
            obj match {
              case Mojiretu(string) => name = string
              case CurrentInputCharacter => {
                name = newEnv.currentInputCharacter match {
                  case CharInput(c) => c.toString
                  case StrInput(s) => s
                  case _ => null
                }
              }
              case _ =>
            }
            var key: String = null
            token match {
              case Variable(v) => key = v
              case CurrentTagToken => key = newEnv.currentTagToken
              case CurrentDOCTYPEToken => key = newEnv.currentDOCTYPEToken
              case _ =>
            }
            newEnv.env.get(key) match {
              case Some(TokenVal(tagToken_(b,n,f,a))) => newEnv.addMap(key, TokenVal(tagToken_(b,n + name,f,a)))
              case Some(TokenVal(DOCTYPEToken(n,f1,f2,a))) => newEnv.addMap(key, TokenVal(DOCTYPEToken(n + name,f1,f2,a)))
              case _ => println("")
            }
          }
          case _ =>
        }
      }
      case Error(error) => {
        newEnv.errorContent = error //println("ErrorCode : "+error)
      }
      case Create(token, corefKey) => {
        val key = if (corefKey == "") "token_" + newEnv.getID() else corefKey
        newEnv.addMap(key, TokenVal(token))
        token match {
          case tagToken_(_,_,_,_) => newEnv.currentTagToken = key
          case DOCTYPEToken(_,_,_,_) => newEnv.currentDOCTYPEToken = key
          case commentToken(_) => newEnv.commentToken = key
          case _ => println("error")
        }
      }
      case Ignore(obj) => println("ignore : " + obj) // 何もしない
      case Flush() => {
        val flushCommand =  If(IsEqual("character reference", "consumed as part of an attribute"),
                              List(Append(TemporaryBuffer, ValueOf(Variable(newEnv.currentAttribute)))),
                               List(Emit(CommandStructure.CharacterToken("code point"))))
        //"code point from the buffer"
        newEnv = interpretCommand(newEnv, flushCommand)
      }
      case Treat() => {
        // AnithingElseのコマンド実行する
      }
      case Start(corefId) => {
        // currentTagTokenに新しい属性を追加する
        val key = if (corefId == "x_-1") "tag_token_attribute_" + newEnv.getID() else corefId
        val newAttribute = new Attribute(null, null)
        newEnv.env.get(newEnv.currentTagToken) match {
          case Some(TokenVal(tagToken_(b, s, f, attributes))) => {
            val newTagToken = tagToken_(b, s, f, attributes :+ key)
            newEnv.addMap(newEnv.currentTagToken, TokenVal(newTagToken))
          }
          case None => println("cant find current tag token")
        }
        newEnv.addMap(key, AttributeVal(newAttribute))
        newEnv.currentAttribute = key
        //newEnv.currentTagToken.attributes :+= new Attribute(null, null)
      }
      case Multiply(obj, by) => {
        obj match {
          case "character reference code" => {
            val mutiNum = by.toInt
          }
          case _ =>
        }
      }
      case Add(obj, to) => {
        obj match {
          case "character reference code" => {
          }
          case _ =>
        }
      }
      case If(bool, t, f) => {
        var comList: List[Command] = null
        if (implementBool(bool)) comList = t else comList = f
        for (c <- comList) newEnv = interpretCommand(newEnv, c)
      }
      case IF_(_) | OTHERWISE_() => println("IF not converted error : " + command)
      case _ => println("undefined command error : " + command)
    }
    newEnv
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

  def getAttributeFromKey(env: Env, attributeList: List[String]): List[Attribute] = {
    attributeList.map(key => {
      env.env.get(key) match {
        case Some(AttributeVal(attribute)) => attribute
        case _ => {println("cant find attribute");new Attribute(null,null)}
      }
    })
  }
}
