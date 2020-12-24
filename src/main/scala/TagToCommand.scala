import CommandStructure._
import Main.txtOut
import TagStructure._

// TagのリストからCommand型に変換する
object TagToCommand {
  var tag_list: List[Tag] = List()

  def toCommand(tagList: List[Tag]): List[Command] = {
    var commandList: List[Command] = List()
    for (tag <- tagList) { commandList ++= RootTag(tag) }
    uniteIf(commandList)
  }

  def RootTag(tag: Tag): List[Command] = {
    var commandList: List[Command] = List()
    tag match {
      case Node(ROOT, List(Node(S, list))) => { commandList ++= STag(Node(S, list)) }
      case _ => txtOut.println("### error_root")
    }
    commandList
  }

  def STag(tag: Tag): List[Command] = {
    var commandList: List[Command] = List()
    tag match {
      case Node(S, list) => {
        list match {
          // S and ...
          case Node(S, s) :: Leaf(CC, Token(_,_, "and")) :: rst => {
            commandList ++= STag(Node(S, s))
            commandList ++= STag(Node(S, rst))
          }
          // S ,and ...
          case Node(S, s) :: Leaf(Comma, _):: Leaf(CC, Token(_,_, "and")) :: rst => {
            commandList ++= STag(Node(S, s))
            commandList ++= STag(Node(S, rst))
          }
          // S ,then ...
          case Node(S, s) :: Leaf(Comma, _) :: Node(ADVP, List(Leaf(RB, Token(_,_, "then")))) :: rst => {
            commandList ++= STag(Node(S, s))
            commandList ++= STag(Node(S, rst))
          }
          // S
          case Node(S, s) :: Nil => {
            commandList ++= STag(Node(S,s))
          }

          // if S ,then ...
          case Node(SBAR, List(Leaf(IN, Token(_,_, "if")), Node(S, bool))) :: Leaf(Comma, _) :: rst => {
            commandList :+= IF_(convertBool(Node(S, bool)))
            commandList ++= STag(Node(S, rst))
          }
          // otherwise, ...
          case Node(ADVP, List(Leaf(RB, Token(_,_, "otherwise")))) :: Leaf(Comma, _) :: rst => {
            commandList :+= OTHERWISE_()
            commandList ++= STag(Node(S, rst))
          }
          // this is ...error
          case Node(NP, List(Leaf(DT, Token(_,_, "this")))) :: Node(VP, List(Leaf(_, Token(_,_, "be")), Node(NP, nplist))) :: Nil => {
            nplist.last match {
              case Leaf(NN, Token(_,_, "error")) => commandList :+= Error(getLeave(removeDT(Node(NP, nplist))))
              case _ =>
            }
          }
          // VP
          case Node(VP, vp) :: Nil => {
            commandList ++= VPTag(Node(VP, vp))
          }
          // then VP
          case Node(ADVP, List(Leaf(RB, Token(_,_, "then")))) :: Node(VP, vp) :: Nil => {
            commandList ++= VPTag(Node(VP, vp))
          }
          case Nil =>
          case _ => txtOut.print("### not match_s : ");txtOut.println(list)
        }
      }
      case _ => txtOut.println("### error_not_s")
    }
    commandList
  }

  def VPTag(tag: Tag): List[Command] = {
    var commandList: List[Command] = List()
    tag match {
      case Node(VP, list) => {
        list match {
          // &文
          case Node(VP, vp1) :: Leaf(CC,Token(_,_,"and")) :: rst => {
            commandList ++= VPTag(Node(VP, vp1))
            commandList ++= STag(Node(S, rst))
          }
          // コンマで繋がっている文
          case Node(VP, vp1) :: Leaf(Comma, _) :: rst => {
            commandList ++= VPTag(Node(VP, vp1))
            commandList ++= STag(Node(S, rst))
          }

          // switch文
          case List(Leaf(VB, Token(_,_,"switch")), Node(PP, List(Leaf(IN, _), Node(NP, np)))) => {
            getLeave(removeDT(Node(NP, np))) match {
              case "return state" => commandList :+= Switch(ReturnState)
              case state => commandList :+= Switch(StateName(state))
            }
          }
          // recomsume文
          case List(Leaf(VB, Token(_,_,"reconsume")), Node(PP, List(Leaf(IN, _), Node(NP, np)))) => {
            getLeave(removeDT(Node(NP, np))) match {
              case "return state" => commandList :+= Reconsume(ReturnState)
              case state => commandList :+= Reconsume(StateName(state))
            }
          }
          case List(Leaf(VB, Token(_,_,"Reconsume")), Node(PP, List(Leaf(IN, _), Node(NP, np)))) => {
            getLeave(removeDT(Node(NP, np))) match {
              case "return state" => commandList :+= Reconsume(ReturnState)
              case state => commandList :+= Reconsume(StateName(state))
            }
          }
          // set(代入する)
          case List(Leaf(VB, Token(_,_,"set")), Node(NP, np1), Node(PP, List(Leaf(IN, _), Node(NP, np2)))) => {
            val i2 = nptagToCommandValue(Node(NP, np2))
            for (n1 <- NPTag(Node(NP, np1))) {
              val i1 = nptagToCommandValue(n1)
              commandList :+= CommandStructure.Set(i1, i2)
            }
          }
          // set2(状態を変更)
          case List(Leaf(VB, Token(_,_,"set")), Node(NP, np), Node(PP, List(Leaf(IN, _), Node(PP, pp)))) => {
            val i1 = nptagToCommandValue(Node(NP, np))
            val i2 = nptagToCommandValue(Node(NP, pp))
            commandList :+= CommandStructure.Set(i1, i2)
          }
          // set3(状態を変更_PPが省略されてるもの)
          case List(Leaf(VB, Token(_,_,"set")), Node(NP, np)) => {
            np match {
              // Set that attribute's name to the current input character, and its value to the empty string.(仮)
              case Node(NP, List(Node(NP, np1_1), Leaf(TO, _), Node(NP, np1_2))) :: Leaf(Comma,_)::Leaf(CC, _)::Node(NP, List(Node(NP, np2_1), Node(PP, List(Leaf(IN, _), Node(NP, np2_2)))))::Nil => {
                val i1 = nptagToCommandValue(Node(NP, np1_1))
                val i2 = nptagToCommandValue(Node(NP, np1_2))
                val i3 = nptagToCommandValue(Node(NP, np2_1))
                val i4 = nptagToCommandValue(Node(NP, np2_2))

                commandList :+= CommandStructure.Set(i1, i2)
                commandList :+= CommandStructure.Set(i3, i4)
              }
              case _ =>{
                val i1 = nptagToCommandValue(Node(NP, np))
                commandList :+= CommandStructure.Set(i1, Non("on"))
              }
            }
          }
          // consume
          case List(Leaf(VB,Token(_,_,"consume")), Node(NP,np)) => {
            for (n <- NPDistribute(Node(NP, np))) commandList :+= CommandStructure.Consume(getLeave(n._1))
          }
          // emit
          case List(Leaf(VB,Token(_,_,"emit")), Node(NP,np)) => {
            for (n <- NPDistribute(Node(NP, np))) {
              commandList :+= Emit(nptagToCommandValue(n._1))
            }
          }
          // emit2
          case List(Leaf(VB,Token(_,_,"emit")), Node(NP,np), Node(PP, pp)) => {
            for (n <- NPDistribute(Node(NP, np))) {
              var token: CommandValue = null
              if (getLeave(n._1).contains("current input character")) token = CurrentInputCharacter
              else if (n._2 != -1) token = Variable("x_" + n._2.toString)
              else token = CommandStructure.CharacterToken(getLeave(n._1))
              commandList :+= Emit(token)
            }
          }
          // ignore
          case List(Leaf(VB,Token(_,_,"ignore")), Node(NP,np)) => {
            for (n <- NPDistribute(Node(NP, np))) commandList :+= Ignore(getLeave(n._1))
          }
          // create
          case List(Leaf(VB,Token(_,_,"create")), Node(NP,np)) => {
            for (n <- NPDistribute(Node(NP, np))) {
              val key = if (n._2 == -1) "" else "x_" + n._2.toString
              var iVal: CommandValue = nptagToCommandValue(n._1)
              iVal match {
                case CurrentDOCTYPEToken => iVal = NewDOCTYPEToken
                case CommentToken => iVal = NewCommentToken
                case _ =>
              }
              commandList :+= Create(iVal, key)
            }
          }
          // multiply
          case List(Leaf(VB,Token(_,_,"multiply")), Node(NP,np1), Node(PP, List(Leaf(IN, Token(_,_, "by")), Node(NP, np2)))) => {
            val i2 = nptagToCommandValue(Node(NP, np2))
            for (n1 <- NPTag(Node(NP, np1))) {
              val i1 = nptagToCommandValue(n1)
              commandList :+= CommandStructure.MultiplyBy(i1, i2)
            }
            //for (n <- NPDistribute(Node(NP, np1))) commandList :+= Multiply(getLeave(n._1), getLeave(Node(NP, np2)))
          }
          // add_1
          case List(Leaf(VB,Token(_,_,"add")), Node(NP,np1), Node(PP, List(Leaf(IN, Token(_,_, "to")), Node(NP, np2)))) => {
            val i2 = nptagToImplementVariable(Node(NP, np2))
            for (n1 <- NPTag(Node(NP, np1))) {
              val i1 = nptagToCommandValue(n1)
              commandList :+= CommandStructure.AddTo(i1, i2)
            }
            //for (n <- NPDistribute(Node(NP, np1))) commandList :+= Add(getLeave(n._1), getLeave(Node(NP, np2)))
          }
          // add_2
          case Leaf(VB, Token(_,_, "add")) :: Node(NP, List(Node(NP, np1), Node(PP, List(Leaf(IN, Token(_,_, "to")), Node(NP, np2))))) :: Nil => {
            val i2 = nptagToImplementVariable(Node(NP, np2))
            for (n1 <- NPTag(Node(NP, np1))) {
              val i1 = nptagToCommandValue(n1)
              commandList :+= CommandStructure.AddTo(i1, i2)
            }
            //for (n <- NPDistribute(Node(NP, np1))) commandList :+= Add(getLeave(n._1), getLeave(Node(NP, np2)))
          }
          // start
          case List(Leaf(VB,Token(_,_,"start")), Node(NP,np), Node(PP, _)) => {
            commandList :+= StartNewAttribute("x_" + getCorefId(Node(NP,np)))
          }
          // treat
          case List(Leaf(VB, Token(_,_, "treat")), Node(NP, _), Node(PP, _), Node(ADVP, _)) | List(Leaf(VB, Token(_,_, "treat")), Node(NP, _), Node(ADVP, _)) => {
            commandList :+= TreatAsAnythingElse()
          }
          // flush
          case List(Leaf(VB,Token(_,_,"flush")), Node(NP,_)) => {
            commandList :+= FlushCodePoint()
          }
          // append_1
          case Leaf(VB, Token(_,_, "append")) :: Node(NP, np1) :: Node(PP, List(Leaf(IN, _), Node(NP, np2))) :: Nil => {
            val i2 = nptagToCommandValue(Node(NP, np2))
            for (n <- NPDistribute(Node(NP, np1))) {
              val i1 = nptagToCommandValue(n._1)
              commandList :+= Append(i1, i2)
            }
          }
          // append_2
          case Leaf(VB, Token(_,_, "append")) :: Node(NP, List(Node(NP, np1), Node(PP, List(Leaf(IN, _), Node(NP, np2))))) :: Nil => {
            val i2 = nptagToCommandValue(Node(NP, np2))
            for (n <- NPDistribute(Node(NP, np1))) {
              val i1 = nptagToCommandValue(n._1)
              commandList :+= Append(i1, i2)
            }
          }
          case _ => txtOut.print("### dont match_vp : ");txtOut.println(list)
        }
      }
      case _ => txtOut.println("### error_not_vp")
    }
    commandList
  }

  // and文を分解する
  def NPTag(tag: Tag): List[Tag] = {
    var taglist: List[Tag] = List()
    tag match {
      case Node(NP, list) => {
        list match {
          case Node(NP, np1) :: Leaf(Comma, _) :: Leaf(CC, Token(_,_, "and")) :: rst => {
            taglist :+= removeDT(Node(NP, np1))
            taglist = taglist ++ NPTag(Node(NP, rst))
          }
          case Node(NP, np1) :: Leaf(Comma, _) :: rst => {
            taglist :+= removeDT(Node(NP, np1))
            taglist = taglist ++ NPTag(Node(NP, rst))
          }
          case Node(NP, np1) :: Leaf(CC, Token(_,_, "and")) :: rst => {
            taglist :+= removeDT(Node(NP, np1))
            taglist = taglist ++ NPTag(Node(NP, rst))
          }
          case Node(NP, np) :: Leaf(NN, nn1) :: Leaf(CC, Token(_,_,"and")) :: Leaf(NN, nn2) :: r => {
            taglist :+= Node(NP, List(Node(NP, np), Leaf(NN, nn1)))
            taglist :+= Node(NP, List(Node(NP, np), Leaf(NN, nn2)))
          }
          case _ => taglist :+= removeDT(tag)
        }
      }
      case _ => txtOut.println("### NpTag error")
    }
    taglist
  }

  def NPDistribute(tag: Tag): List[(Tag, Int)] = {
    var strList: List[(Tag, Int)] = List()
    val taglist = NPTag(tag)
    for(t <- taglist) {
      //strList :+= (toSimpleToken(getLeave(t)), getCorefId(t))
      strList :+= (t, getCorefId(t))
    }
    strList
  }

  def toSimpleToken(token: String): String = {
    val re =  "(U_[0-9A-F][0-9A-F][0-9A-F][0-9A-F]) (.*) character token".r
    re.replaceAllIn(token, m => m.toString().substring(0,6))
  }
  def removeDT(tag: Tag): Tag = {
    tag match {
      case Node(NP, rst) => {
        rst.head match {
          case Leaf(DT, _) => Node(NP, rst.tail)
          //case Leaf(CD, Token(_,_, "two")) => println("aaaa");tag
          case _ => tag
        }
      }
      case _ => tag
    }
  }

  def convertBool(tag: Tag): Bool = {
    tag match {
      case Node(S, list) => {
        list match {
          case Node(S, s1) :: Leaf(CC, Token(_,_, "and")) :: Node(S, s2) :: Nil => {
            And(convertBool(Node(S, s1)), convertBool(Node(S, s2)))
          }
          case Node(S, s1) :: Leaf(CC, Token(_,_, "or")) :: Node(S, s2) :: Nil => {
            Or(convertBool(Node(S, s1)), convertBool(Node(S, s2)))
          }
          case Node(NP, List(Leaf(EX, Token(_,_, "there")))) :: Node(VP, List(Leaf(VB, Token(_,_, "be")), Node(NP, np))) :: Nil => {
            IsExist(getLeave(Node(NP, np)))
          }
          case Node(NP, np1) :: Node(VP, List(Leaf(VB, Token(_,_, "be")), Node(NP, np2))) :: Nil => {
            if (getLeave(Node(NP, np1)).contains("current end tag") && getLeave(Node(NP, np2)).contains("appropriate")) CurrentEndTagIsAppropriate()
            else IsEqual(nptagToCommandValue(Node(NP, np1)), nptagToCommandValue(Node(NP, np2)))
          }
          case Node(NP, List(Leaf(EX, Token(_,_, "there")))) :: Node(VP, List(Leaf(VB, Token(_,_, "be")), Leaf(RB, Token(_,_, "not")),Node(NP, np))) :: Nil => {
            Not(IsExist(getLeave(Node(NP, np))))
          }
          case Node(NP, np1) :: Node(VP, List(Leaf(VB, Token(_,_, "be")), Leaf(RB, Token(_,_, "not")), Node(NP, np2))) :: Nil => {
            if (getLeave(Node(NP, np1)).contains("current end tag") && getLeave(Node(NP, np2)).contains("appropriate")) Not(CurrentEndTagIsAppropriate())
            else Not(IsEqual(nptagToCommandValue(Node(NP, np1)), nptagToCommandValue(Node(NP, np2))))
          }
          case _ => {
            if (getLeave(tag).contains("character reference was consumed as part of an attribute")) CharacterReferenceConsumedAsAttributeVal()
            else UNDEF(getLeave(tag))
          }
        }
      }
      case _ => UNDEF(getLeave(tag))
    }
  }

  // IF文をまとめる
  def uniteIf(commandList: List[Command]): List[Command] = { //listlistにしたい
    var newCommandList: List[Command] = List()
    commandList match {
      case IF_(b) :: rst => {
        val (t, f, e) = distributeTFE(rst)
        newCommandList :+= If(b, t, f)
        newCommandList ++= e
      }
      case command :: rst => newCommandList :+= command;newCommandList ++= uniteIf(rst)
      case Nil =>
    }
    newCommandList
  }
  def distributeTFE(commandList: List[Command]): (List[Command], List[Command], List[Command]) = {
    commandList.indexOf(OTHERWISE_()) match {
      case -1 => (commandList, List(), List())
      case i if (i < commandList.length) => {
        val t = uniteIf(commandList.slice(0, i))
        val fe = uniteIf(commandList.slice(i + 1, commandList.length))
        if (true) {
          (t, fe, List())
        } else {
          (t, fe.slice(0, 1), fe.slice(1, fe.length))
        }
      }
      case _ => (commandList, List(), List())
    }
  }

  def nptagToCommandValue(nptag: Tag): CommandValue = {
    //val str = getLeave(removeDT(nptag))
    val id = getCorefId(nptag)
    nptag match {
      case Node(NP, List(Node(NP, np), Node(PP, List(Leaf(IN, _), Node(NP, np2))))) => {
        val npstr = getLeave(removeDT(Node(NP, np)))
        if (npstr.contains("lowercase version")) {
          return LowerCase(nptagToCommandValue(Node(NP, np2)))
        } else if (npstr.contains("numeric version")) {
          return NumericVersion(nptagToCommandValue(Node(NP, np2)))
        }
      }
      case Node(NP, List(Node(NP, np), Leaf(NN, Token(_,_,n)))) => {
        np.last match {
          case Leaf(POS,Token(_,_,"'s")) => {
            n match {
              case "name" => return NameOf(nptagToCommandValue(Node(NP, np)))
              case "value" => return ValueOf(nptagToCommandValue(Node(NP, np)))
              case _ =>
            }
          }
          case _ =>
        }
      }
      case _ =>
    }
    val str = getLeave(removeDT(nptag))
    if (str.contains("return state")) ReturnState
    else if (str.contains("temporary buffer")) TemporaryBuffer
    else if (str.contains("character reference code")) CharacterReferenceCode
    else if (str.contains("end_of_file")) EndOfFileToken
    else if (str.contains("current input character")) CurrentInputCharacter
    else if(str.contains("_state")) StateName(str)
    else if (str.matches(".*U_[0-9A-F][0-9A-F][0-9A-F][0-9A-F].* character.*")) {
      val unicode: String = "U_[0-9A-F][0-9A-F][0-9A-F][0-9A-F]".r.findFirstIn(str) match {
        case Some(s) => s
        case None => ""
      }
      val char = Utility.unicodeToChar(unicode)
      var num = 1
      if (str.contains("two")) num = 2
      else if (str.contains("three")) num = 3

      var moji  = ""
      while (num >= 1) {
        moji += char
        num += -1
      }
      CString(moji)
    }
    else if (str.contains("new start tag")) {
      NewStartTagToken
    }
    else if (str.contains("new end tag")) {
      NewEndTagToken
    }
    else if (str.contains("DOCTYPE")) {
      if (str.contains("name")) NameOf(CurrentDOCTYPEToken)
      else if (str.contains("new")) NewDOCTYPEToken
      else CurrentDOCTYPEToken
    }
    else if (str.contains("current attribute")) {
      if (str.contains("name")) NameOf(CurrentAttribute)
      else if (str.contains("value")) ValueOf(CurrentAttribute)
      else CurrentAttribute
    }
    else if (str.contains("comment")) {
      if (str.contains("new")) NewCommentToken
      else CommentToken
    }
    else if (str.contains("character token")) CommandStructure.CharacterToken(str)
    else if (str.contains("empty string")) CString("")
    else if (str.contains("current") && str.contains("tag")) {
      if (str.contains("name")) NameOf(CurrentTagToken)
      else CurrentTagToken
    }
    else if (str.contains("name") && id != -1) NameOf(Variable("x_" + id))
    else if (str.contains("value") && id != -1) ValueOf(Variable("x_" + id))
    else if (id != -1) Variable("x_" + id)
    else if (!("0x[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]".r.findFirstIn(str).isEmpty)) {
      "0x[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]".r.findFirstIn(str) match {
        case Some(i) => CInt(Utility.unicodeToInt(i))
        case _ => Non("")
      }
    }
    else if (!("[0-9]+".r.findFirstIn(str).isEmpty)) {
      "[0-9]+".r.findFirstIn(str) match {
        case Some(i) => CInt(i.toInt)
        case _ => Non("")
      }
    }
    else if (!("[sS]tring \".*\"".r.findFirstIn(str).isEmpty)) {
      "[sS]tring \".*\"".r.findFirstIn(str) match {
        case Some(i) => CString(i.slice(8, i.length - 1).replace(" ", ""))
        case _ => Non("")
      }
    }
    else if (str.contains("zero")) CInt(0)
    else Non(str)
  }

  def nptagToImplementVariable(nptag: Tag): ImplementVariable = {
    //val str = getLeave(removeDT(nptag))
    val id = getCorefId(nptag)
    nptag match {
      case Node(NP, List(Node(NP, np), Leaf(NN, Token(_,_,n)))) => {
        np.last match {
          case Leaf(POS,Token(_,_,"'s")) => {
            n match {
              case "name" => return INameOf(nptagToImplementVariable(Node(NP, np)))
              case "value" => return IValueOf(nptagToImplementVariable(Node(NP, np)))
              case _ =>
            }
          }
          case _ =>
        }
      }
      case _ =>
    }
    val str = getLeave(removeDT(nptag))
    if (str.contains("return state")) IReturnState
    else if (str.contains("temporary buffer")) ITemporaryBuffer
    else if (str.contains("character reference code")) ICharacterReferenceCode
    else if (str.contains("DOCTYPE")) {
      if (str.contains("name")) INameOf(ICurrentDOCTYPEToken)
      else ICurrentDOCTYPEToken
    }
    else if (str.contains("current attribute")) {
      if (str.contains("name")) INameOf(ICurrentAttribute)
      else if (str.contains("value")) IValueOf(ICurrentAttribute)
      else ICurrentAttribute
    }
    else if (str.contains("comment")) ICommentToken
    else if (str.contains("current") && str.contains("tag")) {
      if (str.contains("name")) INameOf(ICurrentTagToken)
      else ICurrentTagToken
    }
    else if (str.contains("name") && id != -1) INameOf(IVariable("x_" + id))
    else if (str.contains("value") && id != -1) IValueOf(IVariable("x_" + id))
    else if (id != -1) IVariable("x_" + id)
    else null
  }
}
