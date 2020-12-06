import CommandStructure._
import Environment.characterToken
import Main.txtOut
import TagStructure._

// TagのリストからCommand型に変換する
object TagToCommand {
  var tag_list: List[Tag] = List()

  def toCommand(tagList: List[Tag]): List[Command] = {
    var commandList: List[Command] = List()
    for (tag <- tagList) {
      commandList ++= RootTag(tag)
    }
    uniteIf(commandList)
  }

  def RootTag(tag: Tag): List[Command] = {
    var commandList: List[Command] = List()
    tag match {
      case Node(ROOT, List(Node(S, list))) => {
        commandList ++= STag(Node(S, list))
      }
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
          // S (S)
          case Node(S, s) :: Leaf(LBracket, _) :: Node(S, _) :: Leaf(RBracket, _) :: Nil => {
            commandList ++= STag(Node(S,s))
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
          // &文(途中?)
          case Node(VP, vp1) :: Leaf(CC,Token(_,_,"and")) :: rst => {
            commandList ++= VPTag(Node(VP, vp1))
            commandList ++= STag(Node(S, rst))
          }
          // コンマで繋がっている文
          case Node(VP, vp1) :: Leaf(Comma, _) :: rst => {
            commandList ++= VPTag(Node(VP, vp1))
            //println(rst)
            commandList ++= STag(Node(S, rst))
          }
          // switch文
          case List(Leaf(VB, Token(_,_,"switch")), Node(PP, List(Leaf(IN, _), Node(NP, np)))) => {
            commandList :+= Switch(getLeave(removeDT(Node(NP, np))))
          }
          // recomsume文
          case List(Leaf(VB, Token(_,_,"reconsume")), Node(PP, List(Leaf(IN, _), Node(NP, np)))) => {
            commandList :+= Reconsume(getLeave(removeDT(Node(NP, np))))
          }
          case List(Leaf(VB, Token(_,_,"Reconsume")), Node(PP, List(Leaf(IN, _), Node(NP, np)))) => {
            commandList :+= Reconsume(getLeave(removeDT(Node(NP, np))))
          }
          // set(代入する)
          case List(Leaf(VB, Token(_,_,"set")), Node(NP, np1), Node(PP, List(Leaf(IN, _), Node(NP, np2)))) => {
            commandList :+= CommandStructure.Set((getLeave(Node(NP, np1)), getCorefId(Node(NP, np1))), getLeave(Node(NP, np2)))
          }
          // set2(状態を変更)
          case List(Leaf(VB, Token(_,_,"set")), Node(NP, np), Node(PP, List(Leaf(IN, _), Node(PP, pp)))) => {
            commandList :+= CommandStructure.Set((getLeave(Node(NP, np)), getCorefId(Node(NP, np))), getLeave(Node(PP, pp)))
          }
          // set3(状態を変更_PPが省略されてるもの)
          case List(Leaf(VB, Token(_,_,"set")), Node(NP, np)) => {
            np match {
              // Set that attribute's name to the current input character, and its value to the empty string.(仮)
              case Node(NP, List(Node(NP, np1_1), Leaf(TO, _), Node(NP, np1_2))) :: Leaf(Comma,_)::Leaf(CC, _)::Node(NP, List(Node(NP, np2_1), Node(PP, List(Leaf(IN, _), Node(NP, np2_2)))))::Nil => {
                commandList :+= CommandStructure.Set((getLeave(Node(NP, np1_1)), getCorefId(Node(NP, np1_1))), getLeave(Node(NP, np1_2)))
                commandList :+= CommandStructure.Set((getLeave(Node(NP, np2_1)), getCorefId(Node(NP, np2_1))), getLeave(Node(NP, np2_2)))
              }
              case _ => commandList :+= CommandStructure.Set((getLeave(Node(NP, np)), getCorefId(Node(NP, np))), "on")
            }

          }
          // consume
          case List(Leaf(VB,Token(_,_,"consume")), Node(NP,np)) => {
            for (n <- NPDistribute(Node(NP, np))) commandList :+= CommandStructure.Consume(n._1)
          }
          // emit
          case List(Leaf(VB,Token(_,_,"emit")), Node(NP,np)) => {
            for (n <- NPDistribute(Node(NP, np))) {
              //val key = if (n._2 == -1) "" else "x_" + n._2.toString
              var token: ImplementValue = null
              if (n._1.contains("current tag token")) token = CurrentTagToken
              else if (n._1.contains("DOCTYPE token")) token = CurrentDOCTYPEToken
              else if (n._1.contains("comment token")) token = CommentToken
              else if (n._1.contains("end_of_file")) token = EndOfFileToken
              else if (n._1.contains("current input character")) token = CurrentInputCharacter
              else if (n._2 != -1) token = Variable("x_" + n._2.toString)
              else token = Non(n._1)
              commandList :+= Emit(token)
            }
          }
          // emit2
          case List(Leaf(VB,Token(_,_,"emit")), Node(NP,np), Node(PP, pp)) => {
            for (n <- NPDistribute(Node(NP, np))) {
              var token: ImplementValue = null
              if (n._1.contains("current input character")) token = CurrentInputCharacter
              else if (n._2 != -1) token = Variable("x_" + n._2.toString)
              else token = CommandStructure.CharacterToken(n._1)
              commandList :+= Emit(token)
            }
          }
          // ignore
          case List(Leaf(VB,Token(_,_,"ignore")), Node(NP,np)) => {
            for (n <- NPDistribute(Node(NP, np))) commandList :+= Ignore(n._1)
          }
          // create
          case List(Leaf(VB,Token(_,_,"create")), Node(NP,np)) => {
            for (n <- NPDistribute(Node(NP, np))) {
              val key = if (n._2 == -1) "" else "x_" + n._2.toString
              var token: Environment.Token = null
              if (n._1.contains("start tag token")) token = Environment.tagToken_(true, "", false, List())
              else if (n._1.contains("end tag token")) token = Environment.tagToken_(false, "", false, List())
              else if (n._1.contains("DOCTYPE token")) token = Environment.DOCTYPEToken("", null, null, false)
              else if (n._1.contains("comment token")) token = Environment.commentToken("")
              else println("create_error : " + n._1)
              commandList :+= Create(token, key)
            }
          }
          // multiply
          case List(Leaf(VB,Token(_,_,"multiply")), Node(NP,np1), Node(PP, List(Leaf(IN, Token(_,_, "by")), Node(NP, np2)))) => {
            for (n <- NPDistribute(Node(NP, np1))) commandList :+= Multiply(n._1, getLeave(Node(NP, np2)))
          }
          // add_1
          case List(Leaf(VB,Token(_,_,"add")), Node(NP,np1), Node(PP, List(Leaf(IN, Token(_,_, "to")), Node(NP, np2)))) => {
            for (n <- NPDistribute(Node(NP, np1))) commandList :+= Add(n._1, getLeave(Node(NP, np2)))
          }
          // add_2
          case Leaf(VB, Token(_,_, "add")) :: Node(NP, List(Node(NP, np1), Node(PP, List(Leaf(IN, Token(_,_, "to")), Node(NP, np2))))) :: Nil => {
            for (n <- NPDistribute(Node(NP, np1))) commandList :+= Add(n._1, getLeave(Node(NP, np2)))
          }
          // start
          case List(Leaf(VB,Token(_,_,"start")), Node(NP,np), Node(PP, _)) => {
            commandList :+= Start("x_" + getCorefId(Node(NP,np)))
          }
          // treat
          case List(Leaf(VB, Token(_,_, "treat")), Node(NP, _), Node(PP, _), Node(ADVP, _)) | List(Leaf(VB, Token(_,_, "treat")), Node(NP, _), Node(ADVP, _)) => {
            commandList :+= Treat()
          }
          // flush
          case List(Leaf(VB,Token(_,_,"flush")), Node(NP,_)) => {
            commandList :+= Flush()
          }
          // append_1
          case Leaf(VB, Token(_,_, "append")) :: Node(NP, np1) :: Node(PP, List(Leaf(IN, _), Node(NP, np2))) :: Nil => {
            for (n <- NPDistribute(Node(NP, np1))) commandList :+= Append(n._1, getLeave(Node(NP, np2)))
          }
          // append_2
          case Leaf(VB, Token(_,_, "append")) :: Node(NP, List(Node(NP, np1), Node(PP, List(Leaf(IN, _), Node(NP, np2))))) :: Nil => {
            for (n <- NPDistribute(Node(NP, np1))) commandList :+= Append(n._1, getLeave(Node(NP, np2)))
          }
//          // append_カッコ付き
//          case Leaf(VB, Token(_, "append")) :: Node(NP, np1) :: Node(PRN, _) :: Node(PP, List(Leaf(IN, _), Node(NP, np2))) :: Nil => {
//            commandList :+= Append(NPDistribute(Node(NP, np1)), getLeave(Node(NP, np2)))
//            println("prn")
//          }
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
          case _ => taglist :+= removeDT(tag)
        }
      }
      case _ => txtOut.println("### NpTag error")
    }
    taglist
  }

  def NPDistribute(tag: Tag): List[(String, Int)] = {
    //println(tag)
    var strList: List[(String, Int)] = List()
    val taglist = NPTag(tag)
    for(t <- taglist) {
      strList :+= (toSimpleToken(getLeave(t)), getCorefId(t))
    }
    strList
  }

  def toSimpleToken(token: String): String = {
    val re =  "(U_[0-9A-F][0-9A-F][0-9A-F][0-9A-F]) (.*) character token".r
    re.replaceAllIn(token, m => m.toString().substring(0,6))
  }
  def removeDT(tag: Tag): Tag = {
    //println(tag)
    tag match {
      case Node(NP, rst) => {
        //println(rst.head)
        rst.head match {
          case Leaf(DT, _) => Node(NP, rst.tail)
          case Leaf(CD, Token(_,_, "two")) => println("aaaa");tag
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
            IsEqual(getLeave(Node(NP, np1)), getLeave(Node(NP, np2)))
          }
          case Node(NP, List(Leaf(EX, Token(_,_, "there")))) :: Node(VP, List(Leaf(VB, Token(_,_, "be")), Leaf(RB, Token(_,_, "not")),Node(NP, np))) :: Nil => {
            Not(IsExist(getLeave(Node(NP, np))))
          }
          case Node(NP, np1) :: Node(VP, List(Leaf(VB, Token(_,_, "be")), Leaf(RB, Token(_,_, "not")), Node(NP, np2))) :: Nil => {
            Not(IsEqual(getLeave(Node(NP, np1)), getLeave(Node(NP, np2))))
          }
          case _ => UNDEF(getLeave(tag))
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
}
