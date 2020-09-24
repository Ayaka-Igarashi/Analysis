object TagStructure {
  // tagの種類全部書く
  trait Tag

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
  case class PP$(token: Token) extends Tag // 所有格
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
  case class WP$(token: Token) extends Tag
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

  // Leaf
  case class Token(token: String) extends Tag
}
