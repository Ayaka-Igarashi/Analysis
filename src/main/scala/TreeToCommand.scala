import CommandStructure.{Command, Switch}
import TagStructure._

// parseTreeからCommand型に変換する
object TreeToCommand {
  def toCommand(tag: Tag): Command = {
    var command: Command = null
    tag match {
      case Node(ROOT, List(Node(S,List(node, Leaf(Dot,_))))) => {
        node match {
          // switch文
          case Node(VP, List(Leaf(VB, Token(_,"switch")), Node(PP, List(Leaf(IN, _), Node(NP, np))))) => {
            command = Switch(getLeave(Node(NP, np)))
          }
          case Node(VP, List(Leaf(VB, Token(_,"set")), Node(NP, np1), Node(PP, List(Leaf(IN, _), Node(NP, np2))))) => {
            command = CommandStructure.Set(getLeave(Node(NP, np1)), getLeave(Node(NP, np2)))
          }
          case _ => println("dont match")
        }
      }
      case _ => println("error")
    }
    command
  }
}
