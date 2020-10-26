import CommandStructure._
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
      case _ => println("error_not_root")
    }
    commandList
  }

  def STag(tag: Tag): List[Command] = {
    var commandList: List[Command] = List()
    tag match {
      case Node(S, list) => {
        list match {
          // if文
          case Node(SBAR, List(Leaf(IN, Token(_, "if")), Node(S, l2))) :: Leaf(Comma, _) :: Node(ADVP, List(Leaf(RB, Token(_, "then")))) :: rst => {
            //commandList :+= If(Bool(getLeave(Node(S, l2))), STag(Node(S, rst)), null)
            val com_list = STag(Node(S, rst))
            ifStack.push(If(Bool(getLeave(Node(S, l2))), com_list, null))
          }
          // otherwise
          case Node(ADVP, List(Leaf(RB, Token(_, "otherwise")))) :: Leaf(Comma, _) :: rst => {
            val com_list = STag(Node(S, rst))
            //println(com_list)
            val ifbun = ifStack.pop()
            ifbun.F = com_list
            tag_list :+= Node(S, rst)
            commandList :+= ifbun//If(Bool("otherwise"),null,com_list)
            //println("o")
          }
          // Error文
          case Node(NP, List(Leaf(DT, Token(_, "this")))) :: Node(VP, List(Leaf(_, Token(_, "be")), Node(NP, nplist))) :: Leaf(Dot,_) :: Nil => {
            nplist.last match {
              case Leaf(NN, Token(_, "error")) => commandList :+= Error(getLeave(Node(NP, nplist)))
              case _ =>
            }
          }
          // 命令文
          case node :: Leaf(Dot,_) :: Nil => {
            node match {
              case Node(VP, _) => { commandList ++= VPTag(node)}
              case _ => println("err")
            }
          }
          case node :: Nil => {
            node match {
              case Node(VP, _) => { commandList ++= VPTag(node)}
              case _ => println("err")
            }
          }
          case _ => println("not match_s")
        }
      }
      case _ => println("error_not_s")
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
          // set(代入する)
          case List(Leaf(VB, Token(_,"set")), Node(NP, np1), Node(PP, List(Leaf(IN, _), Node(NP, np2)))) => {
            commandList :+= CommandStructure.Set(getLeave(Node(NP, np1)), getLeave(Node(NP, np2)))
          }
          // set2(状態を変更)
          case List(Leaf(VB, Token(_,"set")), Node(NP, np), Node(PP, List(Leaf(IN, _), Node(PP, pp)))) => {
            commandList :+= CommandStructure.Set(getLeave(Node(NP, np)), getLeave(Node(PP, pp)))
          }
          // consume
          case List(Leaf(VB,Token(_,"consume")), Node(NP,np)) => {
            commandList :+= CommandStructure.Consume(getLeave(Node(NP,np)))
          }
          // emit
          case List(Leaf(VB,Token(_,"emit")), Node(NP,np)) => {
            commandList :+= Emit(getLeave(Node(NP,np)))
          }
          // emit2(途中)
          case List(Leaf(VB,Token(_,"emit")), Node(NP,np), Node(PP, pp)) => {
            commandList :+= Emit(getLeave(Node(NP,np)) + getLeave(Node(PP, pp)))
          }
          // treat(途中)
          case List(Leaf(VB, Token(_, "treat")), Node(NP, _), Node(PP, _), Node(ADVP, _)) => {
            commandList :+= Treat(null,null)
          }
          case _ => println("dont match_vp")
        }
      }
      case _ => println("error_not_vp")
    }
    commandList
  }
}
