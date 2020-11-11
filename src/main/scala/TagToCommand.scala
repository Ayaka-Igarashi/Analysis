import CommandStructure._
import Main.txtOut
import TagStructure._

import scala.collection.mutable

// TagのリストからCommand型に変換する
object TagToCommand {
  val ifStack: mutable.Stack[If] = mutable.Stack()

  var tag_list: List[Tag] = List()

  def toCommand(tagList: List[Tag]): List[Command] = {
    var commandList: List[Command] = List()
    for (tag <- tagList) {
      commandList ++= RootTag(tag)
    }
    commandList
  }

  def RootTag(tag: Tag): List[Command] = {
    var commandList: List[Command] = List()
    tag match {
      case Node(ROOT, List(Node(S, list))) => {
        commandList ++= STag(Node(S, list))
      }
      case _ => txtOut.println("error_root")
    }
    commandList
  }

  def STag(tag: Tag): List[Command] = {
    var commandList: List[Command] = List()
    tag match {
      case Node(S, list) => {
        list match {
          // and
          case Node(S, s1) :: Leaf(CC, Token(_, "and")) :: rst => {
            commandList ++= STag(Node(S, s1))
            commandList ++= STag(Node(S, rst))
          }
          case Node(S, s) :: Leaf(Dot, _) :: Nil => {
            commandList ++= STag(Node(S,s))
          }
          case Node(S, s) :: Nil => {
            commandList ++= STag(Node(S,s))
          }

          // if文
          case Node(SBAR, List(Leaf(IN, Token(_, "if")), Node(S, l2))) :: Leaf(Comma, _) :: Node(ADVP, List(Leaf(RB, Token(_, "then")))) :: rst => {
            //commandList :+= If(Bool(getLeave(Node(S, l2))), STag(Node(S, rst)), null)
            val com_list = STag(Node(S, rst))
            ifStack.push(If(Bool(getLeave(Node(S, l2))), com_list, null))
            //txtOut.println(ifStack)
          }
          // otherwise
          case Node(ADVP, List(Leaf(RB, Token(_, "otherwise")))) :: Leaf(Comma, _) :: rst => {
            val com_list = STag(Node(S, rst))
            //println(com_list)
            if (ifStack.length == 0) {
              txtOut.print("error_otherwise : ");txtOut.println(ifStack)
              commandList :+= If(Bool("otherwise"),null,com_list)
            } else {
              val ifbun = ifStack.pop()
              ifbun.F = com_list
              tag_list :+= Node(S, rst)
              commandList :+= ifbun
            }
            //println("o")
          }
          // Error文(old)
          case Node(NP, List(Leaf(DT, Token(_, "this")))) :: Node(VP, List(Leaf(_, Token(_, "be")), Node(NP, nplist))) :: Leaf(Dot,_) :: Nil => {
            nplist.last match {
              case Leaf(NN, Token(_, "error")) => commandList :+= Error(getLeave(Node(NP, nplist)))
              case _ =>
            }
          }
          // Error
          case Node(NP, List(Leaf(DT, Token(_, "this")))) :: Node(VP, List(Leaf(_, Token(_, "be")), Node(NP, nplist))) :: Nil => {
            nplist.last match {
              case Leaf(NN, Token(_, "error")) => commandList :+= Error(getLeave(Node(NP, nplist)))
              case _ =>
            }
          }
          // 命令文
          case Node(VP, vp) :: Leaf(Dot,_) :: Nil => {
            commandList ++= VPTag(Node(VP, vp))
          }
          case Node(VP, vp) :: Nil => {
            commandList ++= VPTag(Node(VP, vp))
          }
          case _ => txtOut.print("not match_s : ");txtOut.println(list)
        }
      }
      case _ => txtOut.println("error_not_s")
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
            commandList :+= Switch(getLeave(Node(NP, np)))
          }
          // recomsume文
          case List(Leaf(VB, Token(_,"reconsume")), Node(PP, List(Leaf(IN, _), Node(NP, np)))) => {
            commandList :+= Reconsume(getLeave(Node(NP, np)))
          }
          case List(Leaf(VB, Token(_,"Reconsume")), Node(PP, List(Leaf(IN, _), Node(NP, np)))) => {
            commandList :+= Reconsume(getLeave(Node(NP, np)))
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
            commandList :+= CommandStructure.Set(getLeave(Node(NP, np)), "on")
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
          // add
          case List(Leaf(VB,Token(_,"add")), Node(NP,np1), Node(PP, List(Leaf(IN, Token(_, "to")), Node(NP, np2)))) => {
            commandList :+= Add(NPDistribute(Node(NP,np1)), getLeave(Node(NP, np2)))
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
          // append_カッコ付き
          case Leaf(VB, Token(_, "append")) :: Node(NP, np1) :: Node(PRN, _) :: Node(PP, List(Leaf(IN, _), Node(NP, np2))) :: Nil => {
            commandList :+= Append(NPDistribute(Node(NP, np1)), getLeave(Node(NP, np2)))
          }
          case _ => txtOut.print("dont match_vp : ");txtOut.println(list)
        }
      }
      case _ => txtOut.println("error_not_vp")
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
            taglist :+= Node(NP, np1)
            taglist = taglist ++ NPTag(Node(NP, rst))
          }
          case Node(NP, np1) :: Leaf(Comma, _) :: rst => {
            taglist :+= Node(NP, np1)
            taglist = taglist ++ NPTag(Node(NP, rst))
          }
          case Node(NP, np1) :: Leaf(CC, Token(_, "and")) :: rst => {
            taglist ++= NPTag(Node(NP, np1))
            taglist = taglist ++ NPTag(Node(NP, rst))
          }
          case _ => taglist :+= tag
        }
      }
      case _ => println("NpTag error")
    }
    taglist
  }

  def NPDistribute(tag: Tag): String = {
    //println(tag)
    var str = "List ("
    val taglist = NPTag(tag)
    for(t <- taglist) {
      str += getLeave(t)
      str += ", "
    }
    //println(str)
    str + ")"
  }
}
