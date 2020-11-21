import CommandStructure.Command
import TagStructure.Tag

import scala.collection.immutable.ListMap

// htmlを構造化
object StateStructure {
  case class State(var name: String, var prev: String, var trans: List[Trans])
  case class Trans(character: String, process: String)
}

// 自然言語処理した後の構造体(使わない)
object StateParsedStructure {
  case class State_p(var name: String, var prev: List[Tag], var trans: List[Trans_p])
  case class Trans_p(character: String, process: List[Tag])
}

// Command変換した後の構造体
object StateProcessedStructure {
  case class pState(name: String, var prev: List[Command], var trans: ListMap[String, List[Command]])
}