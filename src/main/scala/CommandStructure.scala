// 命令の構造体
object CommandStructure {
  trait Command

  case class Switch(state: String) extends Command
  case class Reconsume(state: String) extends Command

  /**
   * Set文はいろんなパターンがある(大体代入をする命令、対象が様々) -> 同じ関数でいいのか？
   *  Set the temporary buffer to the empty string.
   *  Set its force-quirks flag to on
   * Setするものが複数ある場合はSet自体を複数作成する?
   *  Set that attribute's name to the current input character, and its value to the empty string.
   *  Set that attribute name and value to the empty string.
   * stateのパターン
   *  Set the return state to the data state
   */
  case class Set(obj: String, to: String) extends Command


  // Consume those two characters
  // Consume the next input character
  case class Consume(character: String) extends Command

  /**
   * 1つだけ
   * Emit an end-of-file token.
   * Emit the current input character as a character token.
   * 複数ある場合はEmit自体を複数作る?
   * Emit a U+003C LESS-THAN SIGN character token, a U+002F SOLIDUS character token, and a character token for each of the characters in the temporary buffer (in the order they were added to the buffer).
   * */
  case class Emit(characters: String) extends Command
  case class Error(error: String) extends Command

  // Append the lowercase version of the current input character (add 0x0020 to the character's code point) to the current tag token's tag name.
  // Append a U+FFFD REPLACEMENT CHARACTER character to the current tag token's tag name.
  case class Append(obj: String, to: String) extends Command




  // Create a new start tag token, set its tag name to the empty string.
  // create a comment token whose data is the empty string
  // Create a new DOCTYPE token
  case class Create(token: String) extends Command

  // Ignore the character.
  case class Ignore(obj: String) extends Command

  // Flush code points consumed as a character reference.
  case class Flush() extends Command

  // 1パターンしかないから決め打ちする
  // treat it as per "the character" entry below.
  case class Treat() extends Command
  //Start a new attribute in the current tag token.
  case class Start() extends Command

  // Multiply the character reference code by 16
  case class Multiply(obj: String, by: String) extends Command

  // Add a numeric version of the current input character as a hexadecimal digit (subtract 0x0037 from the character's code point) to the character reference code.
  case class Add(obj: String, to: String) extends Command

  case class If(bool: Bool, var T: List[Command], var F: List[Command]) extends Command

  //
  case class IF(bool: Bool) extends Command
  case class OTHERWISE() extends Command

  // the temporary buffer is the string "script"
  // there is an adjusted current node and it is not an element in the HTML namespace
  // the character reference was consumed as part of an attribute
  /**
   * Boolean
   */
  trait Bool

  case class And(a: Bool, b: Bool) extends Bool
  case class Or(a: Bool, b: Bool) extends Bool
  case class Not(a: Bool) extends Bool
  case class IsEqual(a: String, b: String) extends Bool
  case class IsExist(a: String) extends Bool
  case class UNDEF(str: String) extends Bool

}
