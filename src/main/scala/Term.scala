import TagStructure._

object Term {
  trait Symbol
  case class Word(w: String) extends Symbol
  case class NodePos(p: NodeType) extends Symbol
  case class LeafPos(p: LeafType) extends Symbol

  trait Term
  case class TermVariable(x: String) extends Term
  case class Function(f: Symbol, args: List[Term]) extends Term

  trait Term2
  case class TermVariable2(x: String) extends Term2
  case class Function2(f: Symbol, args: Hedge) extends Term2

  trait HedgeVariableOrTerm // scala3で簡潔に定義したい
  case class HedgeVariable(X: String) extends HedgeVariableOrTerm
  case class T(t: Term) extends HedgeVariableOrTerm
  type Hedge = List[HedgeVariableOrTerm]


  def displayTerm(term: Term): String = {
    term match {
      case TermVariable(x) => x
      case Function(f, args) => {
        var string = ""
        if (args.length == 0) {
          string += displaySymbol(f)
        } else {
          string += (displaySymbol(f) + "(")
          for (arg <- args) {
            string += displayTerm(arg)
            string += ","
          }
          if (string.endsWith(",")) string = string.substring(0, string.length - 1)
          string += ")"
        }
        string
      }
    }
  }

  def displaySymbol(symbol: Symbol): String = {
    symbol match {
      case Word(w) => w
      case NodePos(n) => n.toString
      case LeafPos(l) => l.toString
    }
  }

  def displayTermSeq(terms: List[Term]): String = {
    var string = "["
    for(term <- terms) {
      string += displayTerm(term)
      string += ", "
    }
    if (string.endsWith(", "))  string = string.substring(0, string.length - 2)
    string += "]"
    string
  }

  def displaySubsutitute(subsutitute: List[(TermVariable, List[Term])]): String = {
    var string = "{ "
    for(bind <- subsutitute) {
      string += displayTermSeq(bind._2)
      string += "/"
      string += displayTerm(bind._1)
      string += ", "
    }
    if (string.endsWith(", "))  string = string.substring(0, string.length - 2)
    string += " }"
    string
  }

}
