import CommandStructure.Command
import TagStructure.Tag
import edu.stanford.nlp.coref.data.CorefChain
import edu.stanford.nlp.simple.Token
import edu.stanford.nlp.trees.Tree

import scala.collection.immutable.ListMap

// htmlを構造化
object StateStructure {
  case class State(var name: String, var prev: String, var trans: List[Trans])
  case class Trans(character: String, process: String)
}

// 自然言語処理した後の構造体(中間保存用)
object StateParsedStructure {
  case class nState(var name: String, var prev: (String, String, List[List[(Integer, CorefChain)]], List[Tag]), var trans: List[nTrans])
  case class nTrans(character: String, process: (String, String, List[List[(Integer, CorefChain)]], List[Tag]))
}

// Command変換した後の構造体
object StateProcessedStructure {
  case class pState(name: String, var prev: List[Command], var trans: List[(String, List[Command])])
}