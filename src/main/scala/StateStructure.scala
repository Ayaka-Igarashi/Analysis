import CommandStructure.Command

// htmlを構造化
object StateStructure {
  case class State(var name: String, var prev: String, var trance: List[Trance])
  case class Trance(character: String, process: String)
}

// 自然言語処理した後の構造体
object StateProcessedSturcture {
  case class State_p(name: String, prev: List[Command], trance: List[Trance_p])
  case class Trance_p(character: String, process: List[Command])
}