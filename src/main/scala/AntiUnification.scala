import AUMain.txtOut
import Base.error
import TagStructure._
import Term._

object AntiUnification {
  def antiUnification(tagList: List[Tag]): Term = {
    uniqueId = 0
    val (term,sub) = auTheta(tagList.map(t => conv(t)), List())
    txtOut.println(displaySubsutitute(sub))
    term
  }

  def conv(tag: Tag): Function = {
    tag match {
      case Node(t, list) => {
        Function(NodePos(t), list.map(l => conv(l)))
      }
      case Leaf(t, token) => {
        Function(LeafPos(t), List(Function(Word(token.lemma),List())))
      }
    }
  }

  var uniqueId: Int = 0

  def auTheta(terms: List[Term], substitution: List[(TermVariable, List[Term])]): (Term, List[(TermVariable, List[Term])]) = {
    isAllSame(terms) match {
      case Some(term) => (term, substitution) // pattern(7)
      case None => {
        isAllSameFunction(terms) match {
          case Some((symbol, arity, args)) => { // pattern(8)
            var sub = substitution
            val sList: List[Term] = args.transpose.map(t => {
              val (s, newsub) = auTheta(t, sub)
              sub = newsub
              s
            })
            (Function(symbol, sList), sub)
          }
          case None => {
            isExistBinding(terms, substitution) match {
              case Some(v) => { // pattern(9)
                (v, substitution)
              }
              case None => { // pattern(10)
                val frechVariable = createFreshVariable()
                val newSubstitution: List[(TermVariable, List[Term])] = substitution :+ (frechVariable, terms)
                (frechVariable, newSubstitution)
              }
            }
          }
        }
      }
    }
  }

  /** 補助関数 */
  def isAllSame(terms: List[Term]): Option[Term] = {
    terms match {
      case Nil => error()
      case head :: rst => {
        rst.foreach(t => { if(head != t) return None })
        Some(head)
      }
    }
  }

  def isAllSameFunction(terms: List[Term]): Option[(Symbol, Int, List[List[Term]])] = {
    terms match {
      case Nil => error()
      case Function(symbol, arg) :: rst => {
        val args = rst.map(t => {
          t match {
            case Function(s, a) if (s == symbol && a.length == arg.length) => a
            case _ => return None
          }
        })
        Some((symbol, arg.length, arg :: args))
      }
      case _ => None
    }
  }

  def isExistBinding(terms: List[Term], substitution: List[(TermVariable, List[Term])]): Option[TermVariable] = {
    substitution.foreach(b => {
      if (b._2 == terms) return Some(b._1)
    })
    return None
  }

  def createFreshVariable(): TermVariable = {
    val freshVariable = "z" + uniqueId
    uniqueId += 1
    TermVariable(freshVariable)
  }
}
