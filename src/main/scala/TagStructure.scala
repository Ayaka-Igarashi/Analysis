// parseのデータ型を定義
object TagStructure {
  // tagの種類全部書く
  trait Tag

  trait NodeType
  case class Node(node: NodeType, list: List[Tag]) extends Tag

  trait LeafType
  case class Leaf(leaf: LeafType, token: Token) extends Tag

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
  case object PPD extends LeafType // 所有格(PP$)
  case object RB extends LeafType
  case object RBR extends LeafType
  case object RBS extends LeafType
  case object RP extends LeafType
  case object SYM extends LeafType
  case object TO extends LeafType
  case object UH extends LeafType
  case object VB extends LeafType
  case object VBD extends LeafType
  case object VBG extends LeafType
  case object VBN extends LeafType
  case object VBP extends LeafType
  case object VBZ extends LeafType
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

  // Leaf(とりあえず元の単語と原型両方格納する)
  case class Token(word: String, lemma: String) extends Tag

  /*
  //syntactic tagset
  case class ROOT(list: List[Tag]) extends Tag
  case class ADJP(list: List[Tag]) extends Tag
  case class ADVP(list: List[Tag]) extends Tag
  case class NP(list: List[Tag]) extends Tag
  case class PP(list: List[Tag]) extends Tag
  case class S(list: List[Tag]) extends Tag
  case class SBAR(list: List[Tag]) extends Tag
  case class SBARQ(list: List[Tag]) extends Tag
  case class SINV(list: List[Tag]) extends Tag
  case class SQ(list: List[Tag]) extends Tag
  case class VP(list: List[Tag]) extends Tag
  case class WHADVP(list: List[Tag]) extends Tag
  case class WHNP(list: List[Tag]) extends Tag
  case class WHPP(list: List[Tag]) extends Tag
  case class X(list: List[Tag]) extends Tag
  case class Asterisk(list: List[Tag]) extends Tag // *
  case class Zero(list: List[Tag]) extends Tag // 0
  case class T(list: List[Tag]) extends Tag

  case class NML(list: List[Tag]) extends Tag
  case class FRAG(list: List[Tag]) extends Tag
  case class PRN(list: List[Tag]) extends Tag

  // POS tagset (word)
  case class CC(token: Token) extends Tag
  case class CD(token: Token) extends Tag
  case class DT(token: Token) extends Tag
  case class EX(token: Token) extends Tag
  case class FW(token: Token) extends Tag
  case class IN(token: Token) extends Tag
  case class JJ(token: Token) extends Tag
  case class JJR(token: Token) extends Tag
  case class JJS(token: Token) extends Tag
  case class LS(token: Token) extends Tag
  case class MD(token: Token) extends Tag
  case class NN(token: Token) extends Tag
  case class NNS(token: Token) extends Tag
  case class NNP(token: Token) extends Tag
  case class NNPS(token: Token) extends Tag
  case class PDT(token: Token) extends Tag
  case class POS(token: Token) extends Tag
  case class PRP(token: Token) extends Tag // 主格
  case class PPD(token: Token) extends Tag // 所有格(PP$)
  case class RB(token: Token) extends Tag
  case class RBR(token: Token) extends Tag
  case class RBS(token: Token) extends Tag
  case class RP(token: Token) extends Tag
  case class SYM(token: Token) extends Tag
  case class TO(token: Token) extends Tag
  case class UH(token: Token) extends Tag
  case class VB(token: Token) extends Tag
  case class VBD(token: Token) extends Tag
  case class VBG(token: Token) extends Tag
  case class VBN(token: Token) extends Tag
  case class VBP(token: Token) extends Tag
  case class VBZ(token: Token) extends Tag
  case class WDT(token: Token) extends Tag
  case class WP(token: Token) extends Tag
  case class WPD(token: Token) extends Tag // WP$
  case class WRB(token: Token) extends Tag

  // POS tagset (記号)
  case class Pound(token: Token) extends Tag // #
  case class Dollar(token: Token) extends Tag // $
  case class Dot(token: Token) extends Tag // .
  case class Comma(token: Token) extends Tag // ,
  case class Colon(token: Token) extends Tag // : or ;
  case class LBracket(token: Token) extends Tag // (
  case class RBracket(token: Token) extends Tag // )
  case class DoubleQuote(token: Token) extends Tag // "
  case class LSingleQuote(token: Token) extends Tag
  case class RSingleQuote(token: Token) extends Tag
  case class LDoubleQuote(token: Token) extends Tag // ``
  case class RDoubleQuote(token: Token) extends Tag // ''

  case class HYPH(token: Token) extends Tag

  case class Token(word: String, lemma: String) extends Tag
*/

  // 全ての葉の要素をつなげて取り出す
  def getLeave(tag: Tag): String = {
    getLeave_(tag).tail
  }

  def getLeave_(tag: Tag): String = {
    var str: String = ""
    tag match {
      case Node(_, list) => {
        for (n <- list) {
          str += "_" + getLeave_(n)
        }
      }
      case Leaf(_, Token(word, lem)) => {
        str = word
      }
    }
    str
  }
}
