import CommandStructure.Command
import TagStructure.Tag

// htmlを構造化
object StateStructure {
  case class State(var name: String, var prev: String, var trance: List[Trans])
  case class Trans(character: String, process: String)
}

// 自然言語処理した後の構造体
object StateParsedStructure {
  case class State_p(var name: String, var prev: List[Tag], var trance: List[Trance_p])
  case class Trance_p(character: String, process: List[Tag])
}

// Command変換した後の構造体
object StateProcessedSturcture {
  case class State_f(name: String, prev: List[Command], trance: List[Trance_f])
  case class Trance_f(character: String, process: List[Command])
}