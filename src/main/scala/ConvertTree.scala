import TagStructure._
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.trees.Tree

import Main.txtOut

import scala.collection.JavaConverters._

// CoreNLPのTree型からTag型へ変換する
object ConvertTree {
  // TreeからTag構造体に変換
  def convert(tree: Tree): Tag = {
    tree.value() match {
      // syntactic tag
      case "ROOT" => ROOT(toStruct(tree))
      case "ADJP" => ADJP(toStruct(tree))
      case "ADVP" => ADVP(toStruct(tree))
      case "NP" => NP(toStruct(tree))
      case "PP" => PP(toStruct(tree))
      case "S" => S(toStruct(tree))
      case "SBAR" => SBAR(toStruct(tree))
      case "SBARQ" => SBARQ(toStruct(tree))
      case "SINV" => SINV(toStruct(tree))
      case "SQ" => SQ(toStruct(tree))
      case "VP" =>  VP(toStruct(tree))
      case "WHADVP" => WHADVP(toStruct(tree))
      case "WHNP" => WHNP(toStruct(tree))
      case "WHPP" => WHPP(toStruct(tree))
      case "X" => X(toStruct(tree))
      case "*" => Asterisk(toStruct(tree))
      case "0" => Zero(toStruct(tree))
      case "T" => T(toStruct(tree))

      case "NML" => NML(toStruct(tree))
      case "FRAG" => FRAG(toStruct(tree))
      case "PRN" => PRN(toStruct(tree))

      // pos tag
      case "CC" => CC(toToken(tree))
      case "CD" => CD(toToken(tree))
      case "DT" => DT(toToken(tree))
      case "EX" => EX(toToken(tree))
      case "FW" => FW(toToken(tree))
      case "IN" => IN(toToken(tree))
      case "JJ" => JJ(toToken(tree))
      case "JJR" => JJR(toToken(tree))
      case "JJS" => JJS(toToken(tree))
      case "LS" => LS(toToken(tree))
      case "MD" => MD(toToken(tree))
      case "NN" => NN(toToken(tree))
      case "NNS" => NNS(toToken(tree))
      case "NNP" => NNP(toToken(tree))
      case "NNPS" => NNPS(toToken(tree))
      case "PDT" => PDT(toToken(tree))
      case "POS" => POS(toToken(tree))
      case "PRP" => PRP(toToken(tree))
      case "PP$" => PPD(toToken(tree))
      case "RB" => RB(toToken(tree))
      case "RBR" => RBR(toToken(tree))
      case "RBS" => RBS(toToken(tree))
      case "RP" => RP(toToken(tree))
      case "SYM" => SYM(toToken(tree))
      case "TO" => TO(toToken(tree))
      case "UH" => UH(toToken(tree))
      case "VB" => VB(toToken(tree))
      case "VBD" => VBD(toToken(tree))
      case "VBG" => VBG(toToken(tree))
      case "VBN" => VBN(toToken(tree))
      case "VBP" => VBP(toToken(tree))
      case "VBZ" => VBZ(toToken(tree))
      case "WDT" => WDT(toToken(tree))
      case "WP" => WP(toToken(tree))
      case "WP$" => WPD(toToken(tree))
      case "WRB" => WRB(toToken(tree))
      // pos tag(記号)
      case "#" => Pound(toToken(tree))
      case "$" => Dollar(toToken(tree))
      case "." => Dot(toToken(tree))
      case "," => Comma(toToken(tree))
      case ":" => Colon(toToken(tree))
      case "(" | "-LRB-" => LBracket(toToken(tree))
      case ")" | "-RRB-" => RBracket(toToken(tree))
      case "``" => LDoubleQuote(toToken(tree))
      case "''" => RDoubleQuote(toToken(tree))
      //

      case "HYPH" => HYPH(toToken(tree))
      case _ => txtOut.println(tree.value() + " is not defined");null // error吐くようにする

    }
  }


  // 補助関数(Node)
  def toStruct(tree: Tree): List[Tag] = {
    val treeList = tree.getChildrenAsList.asScala.toList
    var list: List[Tag] = List()
    for(t <- treeList){ list :+= convert(t)}
    list
  }
  // 補助関数(Leaf)
  def toToken(tree: Tree): Token = {
    if (tree.numChildren() != 1) System.out.println("token num error")
    val child = tree.firstChild()
    //child.label()
    //val coreLabel: CoreLabel = new CoreLabel()

    Token(child.value())
  }
}
