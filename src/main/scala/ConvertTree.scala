import TagStructure._
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.trees.Tree

import Main.txtOut

import scala.collection.JavaConverters._

// CoreNLPのTree型からTag型へ変換する
object ConvertTree {
  var tokenList: List[edu.stanford.nlp.simple.Token] = null
  var tokenList2: List[CoreLabel] = null
  var leafDict: Map[Tree, Int] = null

  // TreeからTag構造体に変換
  def convert(tree: Tree): Tag = {

    // 元のTree型のLeafからの距離が1だったら、Leafを作成し
    // それより大きかったらNodeを作成するという風なmatch文にすればより簡潔?(面倒くさい?)
    tree.value() match {
      // syntactic tag
      case "ROOT" => Node(ROOT,toStruct(tree))
      case "ADJP" => Node(ADJP,toStruct(tree))
      case "ADVP" => Node(ADVP,toStruct(tree))
      case "NP" => {
        val c = tree.getChild(0)
        if (c.value() == "PRP" && c.getChild(0).value() == "you") null // "you"を取り除く
        else Node(NP,toStruct(tree))
      }
      case "PP" => Node(PP,toStruct(tree))
      case "S" => Node(S,toStruct(tree))
      case "SBAR" => Node(SBAR,toStruct(tree))
      case "SBARQ" => Node(SBARQ,toStruct(tree))
      case "SINV" => Node(SINV,toStruct(tree))
      case "SQ" => Node(SQ,toStruct(tree))
      case "VP" =>  Node(VP,toStruct(tree))
      case "WHADVP" => Node(WHADVP,toStruct(tree))
      case "WHNP" => Node(WHNP,toStruct(tree))
      case "WHPP" => Node(WHPP,toStruct(tree))
      case "X" => Node(X,toStruct(tree))
      case "*" => Node(Asterisk,toStruct(tree))
      case "0" => Node(Zero,toStruct(tree))
      case "T" => Node(T,toStruct(tree))

      case "NML" => Node(NML,toStruct(tree))
      case "FRAG" => Node(FRAG,toStruct(tree))
      case "PRN" => null//Node(PRN,toStruct(tree)) // カッコの要素を除くならnullにする
      case "INTJ" => Node(INTJ,toStruct(tree))

      // pos tag
      case "CC" => Leaf(CC,toToken(tree))
      case "CD" => Leaf(CD,toToken(tree))
      case "DT" => Leaf(DT,toToken(tree))
      case "EX" => Leaf(EX,toToken(tree))
      case "FW" => Leaf(FW,toToken(tree))
      case "IN" => Leaf(IN,toToken(tree))
      case "JJ" => Leaf(JJ,toToken(tree))
      case "JJR" => Leaf(JJR,toToken(tree))
      case "JJS" => Leaf(JJS,toToken(tree))
      case "LS" => Leaf(LS,toToken(tree))
      case "MD" => Leaf(MD,toToken(tree))
      case "NN" => Leaf(NN,toToken(tree))
      case "NNS" => Leaf(NNS,toToken(tree))
      case "NNP" => Leaf(NNP,toToken(tree))
      case "NNPS" => Leaf(NNPS,toToken(tree))
      case "PDT" => Leaf(PDT,toToken(tree))
      case "POS" => Leaf(POS,toToken(tree))
      case "PRP" => Leaf(PRP,toToken(tree))
      case "PRP$" => Leaf(PRPD,toToken(tree))
      case "PP$" => Leaf(PPD,toToken(tree))
      case "RB" => Leaf(RB,toToken(tree))
      case "RBR" => Leaf(RBR,toToken(tree))
      case "RBS" => Leaf(RBS,toToken(tree))
      case "RP" => Leaf(RP,toToken(tree))
      case "SYM" => Leaf(SYM,toToken(tree))
      case "TO" => Leaf(TO,toToken(tree))
      case "UH" => Leaf(UH,toToken(tree))
      case "VB"|"VBD"|"VBG"|"VBN"|"VBP"|"VBZ" => Leaf(VB,toToken(tree)) // 動詞は全部同じく扱う
//      case "VBD" => Leaf(VBD,toToken(tree))
//      case "VBG" => Leaf(VBG,toToken(tree))
//      case "VBN" => Leaf(VBN,toToken(tree))
//      case "VBP" => Leaf(VBP,toToken(tree))
//      case "VBZ" => Leaf(VBZ,toToken(tree))
      case "WDT" => Leaf(WDT,toToken(tree))
      case "WP" => Leaf(WP,toToken(tree))
      case "WP$" => Leaf(WPD,toToken(tree))
      case "WRB" => Leaf(WRB,toToken(tree))
      // pos tag(記号)
      case "#" => Leaf(Pound,toToken(tree))
      case "$" => Leaf(Dollar,toToken(tree))
      case "." => Leaf(Dot,toToken(tree))
      case "," => Leaf(Comma,toToken(tree))
      case ":" => Leaf(Colon,toToken(tree))
      case "(" | "-LRB-" => Leaf(LBracket,toToken(tree))
      case ")" | "-RRB-" => Leaf(RBracket,toToken(tree))
      case "``" => Leaf(LDoubleQuote,toToken(tree))
      case "''" => Leaf(RDoubleQuote,toToken(tree))
      //

      case "HYPH" => Leaf(HYPH,toToken(tree))
      case "NFP" => Leaf(NFP,toToken(tree))
      case "GW" =>Leaf(GW,toToken(tree))
      case _ => txtOut.println(tree.value() + " is not defined : " + tree.numChildren());null // error吐くようにする

    }
  }


  // 補助関数(Node)
  def toStruct(tree: Tree): List[Tag] = {
    val treeList = tree.getChildrenAsList.asScala.toList
    var list: List[Tag] = List()
    for(t <- treeList){
      val tag = convert(t)
      if (tag != null) list :+= tag
    }
    list
  }
  // 補助関数(Leaf)
  def toToken(tree: Tree): Token = {
    if (tree.numChildren() != 1) System.out.println("not leaf error")
    val child = tree.firstChild()
    leafDict.get(child) match {
      case Some(i) => Token(child.value(), tokenList(i).lemma())
      case None => Token(child.value(), null) // error
    }
  }

  // lemmaツリーを作るのに使う関数
  def makeLeafMap(tree: Tree) = {
    leafDict = Map()
    var i = 0
    val leaveList: List[Tree] = tree.getLeaves().asScala.toList
    for (leaf <- leaveList) {
      leafDict += (leaf -> i)
      i += 1
    }
  }

}
