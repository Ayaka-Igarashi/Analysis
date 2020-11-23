import CommandStructure._
import Main.txtOut
import TagStructure._

import scala.collection.mutable

// TagのリストからCommand型に変換する
object TagToCommand {
  val ifStack: mutable.Stack[If] = mutable.Stack()

  var tag_list: List[Tag] = List()

  var state_if: Int = 0 //0: None, 1: then phrase, 2: otherwise phrase

  def toCommand(tagList: List[Tag]): List[Command] = {
    state_if = 0
    var commandList: List[Command] = List()
    for (tag <- tagList) {
      val state = state_if
//      val com = RootTag(tag)
      if (state == 1) {
        commandList ++= RootTag(tag)

//        if (ifStack.length == 0) {txtOut.print("### error_rr : ");commandList ++= RootTag(tag) }
//        else {
//          val ifbun = ifStack.pop()
//          val com = RootTag(tag)
//          ifbun.T ++= com
//          ifStack.push(ifbun)
//        }
      } else {
        val com = RootTag(tag)
        commandList ++= com
      }
    }
    commandList
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
          case Node(S, s) :: Leaf(CC, Token(_, "and")) :: rst => {
            commandList ++= STag(Node(S, s))
            if (state_if == 1) {
              if (ifStack.length == 0) {
                val com = STag(Node(S, rst))
                commandList ++= com
              }else {
                val ifbun = ifStack.pop()
                val com = STag(Node(S, rst))
                ifbun.T ++= com
                ifStack.push(ifbun)
              }
            }
            else {
              val com = STag(Node(S, rst))
              commandList ++= com
            }
          }
          // S ,and ...
          case Node(S, s) :: Leaf(Comma, _):: Leaf(CC, Token(_, "and")) :: rst => {
            commandList ++= STag(Node(S, s))
            if (state_if == 1) {
              if (ifStack.length == 0) {
                val com = STag(Node(S, rst))
                commandList ++= com
              } else {
                val ifbun = ifStack.pop()
                val com = STag(Node(S, rst))
                ifbun.T ++= com
                ifStack.push(ifbun)
              }
            }
            else {
              val com = STag(Node(S, rst))
              commandList ++= com
            }
          }
          // S ,then ...
          case Node(S, s) :: Leaf(Comma, _) :: Node(ADVP, List(Leaf(RB, Token(_, "then")))) :: rst => {
            commandList ++= STag(Node(S, s))
            if (state_if == 1) {
              if (ifStack.length == 0) {
                val com = STag(Node(S, rst))
                commandList ++= com
              }else {
                val ifbun = ifStack.pop()
                val com = STag(Node(S, rst))
                ifbun.T ++= com
                ifStack.push(ifbun)
              }
            }
            else {
              val com = STag(Node(S, rst))
              commandList ++= com
            }
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
          case Node(SBAR, List(Leaf(IN, Token(_, "if")), Node(S, bool))) :: Leaf(Comma, _) :: rst => {
            state_if = 1
            val com_list = STag(Node(S, rst))
            ifStack.push(If(convertBool(Node(S, bool)), com_list, null))
            //txtOut.println(ifStack)
          }
          // otherwise, ...
          case Node(ADVP, List(Leaf(RB, Token(_, "otherwise")))) :: Leaf(Comma, _) :: rst => {
            state_if = 2

            //println(com_list)
            if (ifStack.length == 0) {
              txtOut.print("### error_otherwise : ");txtOut.println(ifStack)
              val com_list = STag(Node(S, rst))
              commandList :+= If(UNDEF("otherwise"),null,com_list)
            } else {
              val ifbun = ifStack.pop()
              val com_list = STag(Node(S, rst))
              ifbun.F = com_list
              //tag_list :+= Node(S, rst)
              commandList :+= ifbun
            }
          }
          // this is ...error
          case Node(NP, List(Leaf(DT, Token(_, "this")))) :: Node(VP, List(Leaf(_, Token(_, "be")), Node(NP, nplist))) :: Nil => {
            nplist.last match {
              case Leaf(NN, Token(_, "error")) => commandList :+= Error(getLeave(removeDT(Node(NP, nplist))))
              case _ =>
            }
          }
          // VP
          case Node(VP, vp) :: Nil => {
            commandList ++= VPTag(Node(VP, vp))
          }
          // then VP
          case Node(ADVP, List(Leaf(RB, Token(_, "then")))) :: Node(VP, vp) :: Nil => {
            commandList ++= VPTag(Node(VP, vp))
          }
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
          case Node(VP, vp1) :: Leaf(CC,Token(_,"and")) :: rst => {
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
          case List(Leaf(VB, Token(_,"switch")), Node(PP, List(Leaf(IN, _), Node(NP, np)))) => {
            commandList :+= Switch(getLeave(removeDT(Node(NP, np))))
          }
          // recomsume文
          case List(Leaf(VB, Token(_,"reconsume")), Node(PP, List(Leaf(IN, _), Node(NP, np)))) => {
            commandList :+= Reconsume(getLeave(removeDT(Node(NP, np))))
          }
          case List(Leaf(VB, Token(_,"Reconsume")), Node(PP, List(Leaf(IN, _), Node(NP, np)))) => {
            commandList :+= Reconsume(getLeave(removeDT(Node(NP, np))))
          }
          // set(代入する)
          case List(Leaf(VB, Token(_,"set")), Node(NP, np1), Node(PP, List(Leaf(IN, _), Node(NP, np2)))) => {
            commandList :+= CommandStructure.Set(getLeave(Node(NP, np1)), getLeave(Node(NP, np2)))
          }
          // set2(状態を変更)
          case List(Leaf(VB, Token(_,"set")), Node(NP, np), Node(PP, List(Leaf(IN, _), Node(PP, pp)))) => {
            commandList :+= CommandStructure.Set(getLeave(Node(NP, np)), getLeave(Node(PP, pp)))
          }
          // set3(状態を変更_PPが省略されてるもの)
          case List(Leaf(VB, Token(_,"set")), Node(NP, np)) => {
            np match {
              // Set that attribute's name to the current input character, and its value to the empty string.(仮)
              case Node(NP, List(Node(NP, np1_1), Leaf(TO, _), Node(NP, np1_2))) :: Leaf(Comma,_)::Leaf(CC, _)::Node(NP, List(Node(NP, np2_1), Node(PP, List(Leaf(IN, _), Node(NP, np2_2)))))::Nil => {
                commandList :+= CommandStructure.Set(getLeave(Node(NP, np1_1)), getLeave(Node(NP, np1_2)))
                commandList :+= CommandStructure.Set(getLeave(Node(NP, np2_1)), getLeave(Node(NP, np2_2)))
              }
              case _ => commandList :+= CommandStructure.Set(getLeave(Node(NP, np)), "on")
            }

          }
          // consume
          case List(Leaf(VB,Token(_,"consume")), Node(NP,np)) => {
            commandList :+= CommandStructure.Consume(NPDistribute(Node(NP,np)))
          }
          // emit
          case List(Leaf(VB,Token(_,"emit")), Node(NP,np)) => {
            commandList :+= Emit(NPDistribute(Node(NP, np)))//Emit(getLeave(Node(NP,np)))
          }
          // emit2(途中)
          case List(Leaf(VB,Token(_,"emit")), Node(NP,np), Node(PP, pp)) => {
            commandList :+= Emit(NPDistribute(Node(NP,np)) +"_"+ getLeave(Node(PP, pp)))
          }
          // ignore
          case List(Leaf(VB,Token(_,"ignore")), Node(NP,np)) => {
            commandList :+= Ignore(NPDistribute(Node(NP,np)))
          }
          // create
          case List(Leaf(VB,Token(_,"create")), Node(NP,np)) => {
            commandList :+= Create(NPDistribute(Node(NP,np)))
          }
          // multiply
          case List(Leaf(VB,Token(_,"multiply")), Node(NP,np1), Node(PP, List(Leaf(IN, Token(_, "by")), Node(NP, np2)))) => {
            commandList :+= Multiply(NPDistribute(Node(NP,np1)), getLeave(Node(NP, np2)))
          }
          // add_1
          case List(Leaf(VB,Token(_,"add")), Node(NP,np1), Node(PP, List(Leaf(IN, Token(_, "to")), Node(NP, np2)))) => {
            commandList :+= Add(NPDistribute(Node(NP,np1)), getLeave(Node(NP, np2)))
          }
          // add_2
          case Leaf(VB, Token(_, "add")) :: Node(NP, List(Node(NP, np1), Node(PP, List(Leaf(IN, Token(_, "to")), Node(NP, np2))))) :: Nil => {
            commandList :+= Add(NPDistribute(Node(NP, np1)), getLeave(Node(NP, np2)))
          }
          // start
          case List(Leaf(VB,Token(_,"start")), Node(NP,_), Node(PP, _)) => {
            commandList :+= Start()
          }
          // treat
          case List(Leaf(VB, Token(_, "treat")), Node(NP, _), Node(PP, _), Node(ADVP, _)) | List(Leaf(VB, Token(_, "treat")), Node(NP, _), Node(ADVP, _)) => {
            commandList :+= Treat()
          }
          // flush
          case List(Leaf(VB,Token(_,"flush")), Node(NP,_)) => {
            commandList :+= Flush()
          }
          // append_1
          case Leaf(VB, Token(_, "append")) :: Node(NP, np1) :: Node(PP, List(Leaf(IN, _), Node(NP, np2))) :: Nil => {
            commandList :+= Append(NPDistribute(Node(NP, np1)), getLeave(Node(NP, np2)))
          }
          // append_2
          case Leaf(VB, Token(_, "append")) :: Node(NP, List(Node(NP, np1), Node(PP, List(Leaf(IN, _), Node(NP, np2))))) :: Nil => {
            commandList :+= Append(NPDistribute(Node(NP, np1)), getLeave(Node(NP, np2)))
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
          case Node(NP, np1) :: Leaf(Comma, _) :: Leaf(CC, Token(_, "and")) :: rst => {
            taglist :+= removeDT(Node(NP, np1))
            taglist = taglist ++ NPTag(Node(NP, rst))
          }
          case Node(NP, np1) :: Leaf(Comma, _) :: rst => {
            taglist :+= removeDT(Node(NP, np1))
            taglist = taglist ++ NPTag(Node(NP, rst))
          }
          case Node(NP, np1) :: Leaf(CC, Token(_, "and")) :: rst => {
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

  def NPDistribute(tag: Tag): String = {
    //println(tag)
    var str = "List ("
    val taglist = NPTag(tag)
    for(t <- taglist) {
      str += toSimpleToken(getLeave(t))
      str += ", "
    }
    //println(str)
    str + ") "
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
          case Node(S, s1) :: Leaf(CC, Token(_, "and")) :: Node(S, s2) :: Nil => {
            And(convertBool(Node(S, s1)), convertBool(Node(S, s2)))
          }
          case Node(S, s1) :: Leaf(CC, Token(_, "or")) :: Node(S, s2) :: Nil => {
            Or(convertBool(Node(S, s1)), convertBool(Node(S, s2)))
          }
          case Node(NP, List(Leaf(EX, Token(_, "there")))) :: Node(VP, List(Leaf(VB, Token(_, "be")), Node(NP, np))) :: Nil => {
            IsExist(getLeave(Node(NP, np)))
          }
          case Node(NP, np1) :: Node(VP, List(Leaf(VB, Token(_, "be")), Node(NP, np2))) :: Nil => {
            IsEqual(getLeave(Node(NP, np1)), getLeave(Node(NP, np2)))
          }
          case Node(NP, List(Leaf(EX, Token(_, "there")))) :: Node(VP, List(Leaf(VB, Token(_, "be")), Leaf(RB, Token(_, "not")),Node(NP, np))) :: Nil => {
            Not(IsExist(getLeave(Node(NP, np))))
          }
          case Node(NP, np1) :: Node(VP, List(Leaf(VB, Token(_, "be")), Leaf(RB, Token(_, "not")), Node(NP, np2))) :: Nil => {
            Not(IsEqual(getLeave(Node(NP, np1)), getLeave(Node(NP, np2))))
          }
          case _ => UNDEF(getLeave(tag))
        }
      }
      case _ => UNDEF(getLeave(tag))
    }
  }

}
