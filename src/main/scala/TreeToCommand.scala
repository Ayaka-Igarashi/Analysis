import CommandStructure._
import TagStructure._

import scala.collection.mutable

// parseTreeからCommand型に変換する
object TreeToCommand {
  val ifStack: mutable.Stack[If] = mutable.Stack()

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
      case _ => println("error")
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
            ifStack.push(If(Bool(getLeave(Node(S, l2))), STag(Node(S, rst)), null))
          }
          // otherwise
          case Node(ADVP, List(Leaf(RB, Token(_, "otherwise")))) :: Leaf(Comma, _) :: rst => {
            val ifbun = ifStack.pop()
            //ifbun.F = STag(Node(S, rst))
            //println(ifbun.F)
            println(rst)
            commandList :+= If(Bool("otherwise"),null,STag(Node(S, rst)))
            println("o")
          }
          // その他
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
      case _ => println("error")
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
          // set
          case List(Leaf(VB, Token(_,"set")), Node(NP, np1), Node(PP, List(Leaf(IN, _), Node(NP, np2)))) => {
            commandList :+= CommandStructure.Set(getLeave(Node(NP, np1)), getLeave(Node(NP, np2)))
          }
          // consume
          case List(Leaf(VB,Token(_,"consume")), Node(NP,np)) => {
            commandList :+= CommandStructure.Consume(getLeave(Node(NP,np)))
          }
          // treat(途中)
          case List(Leaf(VB, Token(_, "treat")), Node(NP, _), Node(PP, _), Node(ADVP, _)) => {
            commandList :+= Treat(null,null)
          }
          case _ => println("dont match_vp")
        }
      }
      case _ =>
    }
    commandList
  }
}
