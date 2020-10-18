import CommandStructure.Command

// htmlを構造化
object StateStructure {
  case class State(name: String, prev: String, trance: List[Trance])
  case class Trance(character: String, process: String)
}

// 自然言語処理した後の構造体
object StateProcessedSturcture {
  case class State_(name: String, prev: List[Command], trance: List[Trance_])
  case class Trance_(character: String, process: List[Command])
}