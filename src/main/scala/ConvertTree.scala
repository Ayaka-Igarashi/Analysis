import TagStructure._
import edu.stanford.nlp.trees.Tree
import scala.collection.JavaConverters._

class ConvertTree {
  def convert(tree: Tree): Tag = {
    tree.label().value() match {
      case "ROOT" => ROOT(treelistToStruct(tree.getChildrenAsList.asScala.toList))
      case "ADJP" => ADJP(treelistToStruct(tree.getChildrenAsList.asScala.toList))
      case "ADVP" => ADVP(treelistToStruct(tree.getChildrenAsList.asScala.toList))
      case "NP" => NP(treelistToStruct(tree.getChildrenAsList.asScala.toList))
      case "PP" => PP(treelistToStruct(tree.getChildrenAsList.asScala.toList))
      case "S" => S(treelistToStruct(tree.getChildrenAsList.asScala.toList))
      case "SBAR" => SBAR(treelistToStruct(tree.getChildrenAsList.asScala.toList))
      case "SBARQ" => SBARQ(treelistToStruct(tree.getChildrenAsList.asScala.toList))
      case "SINV" => SINV(treelistToStruct(tree.getChildrenAsList.asScala.toList))
      case "SQ" => SQ(treelistToStruct(tree.getChildrenAsList.asScala.toList))
      case "VP" =>  VP(treelistToStruct(tree.getChildrenAsList.asScala.toList))
      case "WHADVP" => WHADVP(treelistToStruct(tree.getChildrenAsList.asScala.toList))
      case "WHNP" => WHNP(treelistToStruct(tree.getChildrenAsList.asScala.toList))
      case "WHPP" => WHPP(treelistToStruct(tree.getChildrenAsList.asScala.toList))
      case "X" => X(treelistToStruct(tree.getChildrenAsList.asScala.toList))
      case "*" => Asterisk(treelistToStruct(tree.getChildrenAsList.asScala.toList))
      case "0" => Zero(treelistToStruct(tree.getChildrenAsList.asScala.toList))
      case "T" => T(treelistToStruct(tree.getChildrenAsList.asScala.toList))

      case "CC" => CC(getToken(tree.firstChild()))
      case "CD" => CD(getToken(tree.firstChild()))
      case "DT" => DT(getToken(tree.firstChild()))
      case "EX" => EX(getToken(tree.firstChild()))
      case "FW" => FW(getToken(tree.firstChild()))
      case "IN" => IN(getToken(tree.firstChild()))
      case "JJ" => JJ(getToken(tree.firstChild()))
      case "JJR" => JJR(getToken(tree.firstChild()))
      case "JJS" => JJS(getToken(tree.firstChild()))
      case "LS" => LS(getToken(tree.firstChild()))
      case "MD" => MD(getToken(tree.firstChild()))
      case "NN" => NN(getToken(tree.firstChild()))
      case "NNS" => NNS(getToken(tree.firstChild()))
      case "NNP" => NNP(getToken(tree.firstChild()))
      case "NNPS" => NNPS(getToken(tree.firstChild()))
      case "PDT" => PDT(getToken(tree.firstChild()))
      case "POS" => POS(getToken(tree.firstChild()))
      case "PRP" => PRP(getToken(tree.firstChild()))
      case "PP$" => PP$(getToken(tree.firstChild()))
      case "RB" => RB(getToken(tree.firstChild()))
      case "RBR" => RBR(getToken(tree.firstChild()))
      case "RBS" => RBS(getToken(tree.firstChild()))
      case "RP" => RP(getToken(tree.firstChild()))
      case "SYM" => SYM(getToken(tree.firstChild()))
      case "TO" => TO(getToken(tree.firstChild()))
      case "UH" => UH(getToken(tree.firstChild()))
      case "VB" => {
        VB(getToken(tree.firstChild()))
        /*
        val child: Tree = tree.firstChild()
        val token: Token = Token(child.value())
        VB(token)
        */
      }
      case "VBD" => VBD(getToken(tree.firstChild()))
      case "VBG" => VBG(getToken(tree.firstChild()))
      case "VBN" => VBN(getToken(tree.firstChild()))
      case "VBP" => VBP(getToken(tree.firstChild()))
      case "VBZ" => VBZ(getToken(tree.firstChild()))
      case "WDT" => WDT(getToken(tree.firstChild()))
      case "WP" => WP(getToken(tree.firstChild()))
      case "WP$" => WP$(getToken(tree.firstChild()))
      case "WRB" => WRB(getToken(tree.firstChild()))
      case "#" => Pound(getToken(tree.firstChild()))
      case "$" => Dollar(getToken(tree.firstChild()))
      case "." => Dot(getToken(tree.firstChild()))
      case "," => Comma(getToken(tree.firstChild()))
      case ":" => Colon(getToken(tree.firstChild()))
      case "(" => LBracket(getToken(tree.firstChild()))
      case ")" => RBracket(getToken(tree.firstChild()))
      case _ => null

    }
  }


  // 補助関数
  def treelistToStruct(treeList: List[Tree]): List[Tag] = {
    var list: List[Tag] = List()
    for(t <- treeList){ list :+= convert(t)}
    list
  }
  // 補助関数
  def getToken(tree: Tree): Token = {
    Token(tree.value())
  }
}
