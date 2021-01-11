// parseのデータ型を定義
object TagStructure {
  // tagの種類全部書く
  trait Tag

  trait NodeType
  case class Node(node: NodeType, list: List[Tag]) extends Tag
  trait LeafType
  case class Leaf(leaf: LeafType, token: Token_) extends Tag

  //syntactic tagset
  case object ROOT extends NodeType
  case object ADJP extends NodeType
  case object ADVP extends NodeType
  case object NP extends NodeType
  case object PP extends NodeType
  case object S extends NodeType
  case object SBAR extends NodeType
  case object SBARQ extends NodeType
  case object SINV extends NodeType
  case object SQ extends NodeType
  case object VP extends NodeType
  case object WHADVP extends NodeType
  case object WHNP extends NodeType
  case object WHPP extends NodeType
  case object X extends NodeType
  case object Asterisk extends NodeType // *
  case object Zero extends NodeType // 0
  case object T extends NodeType

  case object NML extends NodeType
  case object FRAG extends NodeType
  case object PRN extends NodeType
  case object INTJ extends NodeType
  case object UCP extends NodeType
  case object QP extends NodeType

  // POS tagset (word)
  case object CC extends LeafType
  case object CD extends LeafType
  case object DT extends LeafType
  case object EX extends LeafType
  case object FW extends LeafType
  case object IN extends LeafType
  case object JJ extends LeafType
  case object JJR extends LeafType
  case object JJS extends LeafType
  case object LS extends LeafType
  case object MD extends LeafType
  case object NN extends LeafType
  case object NNS extends LeafType
  case object NNP extends LeafType
  case object NNPS extends LeafType
  case object PDT extends LeafType
  case object POS extends LeafType
  case object PRP extends LeafType // 主格
  case object PRPD extends LeafType // PRP$
  case object PPD extends LeafType // 所有格(PP$)
  case object RB extends LeafType
  case object RBR extends LeafType
  case object RBS extends LeafType
  case object RP extends LeafType
  case object SYM extends LeafType
  case object TO extends LeafType
  case object UH extends LeafType
  case object VB extends LeafType
//  case object VBD extends LeafType
//  case object VBG extends LeafType
//  case object VBN extends LeafType
//  case object VBP extends LeafType
//  case object VBZ extends LeafType
  case object WDT extends LeafType
  case object WP extends LeafType
  case object WPD extends LeafType // WP$
  case object WRB extends LeafType
  // POS tagset (記号)
  case object Pound extends LeafType // #
  case object Dollar extends LeafType // $
  case object Dot extends LeafType // .
  case object Comma extends LeafType // ,
  case object Colon extends LeafType // : or ;
  case object LBracket extends LeafType // (
  case object RBracket extends LeafType // )
  case object DoubleQuote extends LeafType // "
  case object LSingleQuote extends LeafType
  case object RSingleQuote extends LeafType
  case object LDoubleQuote extends LeafType // ``
  case object RDoubleQuote extends LeafType // ''

  case object HYPH extends LeafType
  case object NFP extends LeafType
  case object GW extends LeafType

  // Leaf(とりあえず元の単語と原型両方格納する)
  case class Token_(rel: Set[(String, Int)], ref: Int, word: String, lemma: String) extends Tag
  case class Token(ref: Int, word: String, lemma: String) extends Tag

  // 全ての葉の要素をつなげて取り出す
  def getLeave(tag: Tag): String = {
    getLeave_(tag)
  }

  def getLeave_(tag: Tag): String = {
    var str: String = ""
    tag match {
      case Node(_, list) => {
        for (n <- list) {
          if (str.length > 0 && str.last != ' ') str += " "
          str += getLeave_(n)
        }
      }
      case Leaf(_, Token_(_,_, word, lem)) => {
        str = word
      }

      case _ => print("matchERROR: ");println(tag)
    }
    str
  }

  def getCorefId(tag: Tag): Int = {
    tag match {
      case Node(_, list) => getCorefId(list.head)
      case Leaf(_, Token_(_,id, _, _)) => id
    }
  }
}
