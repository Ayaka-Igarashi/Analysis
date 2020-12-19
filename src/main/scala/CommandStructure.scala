// 命令の構造体
object CommandStructure {
  trait Command

  case class Switch(state: StateVal) extends Command
  case class Reconsume(state: StateVal) extends Command

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
  case class Set(obj: ImplementValue, to: ImplementValue) extends Command

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
  case class Emit(token: ImplementValue) extends Command

  case class Error(error: String) extends Command

  // Append the lowercase version of the current input character (add 0x0020 to the character's code point) to the current tag token's tag name.
  // Append a U+FFFD REPLACEMENT CHARACTER character to the current tag token's tag name.
  case class Append(obj: ImplementValue, to: ImplementValue) extends Command

  // create a comment token whose data is the empty string
  case class Create(token: Environment.Token, valueKey: String) extends Command

  // Ignore the character.
  case class Ignore(obj: String) extends Command

  // 1パターンしかないから決め打ちする
  case class Flush() extends Command // Flush code points consumed as a character reference.
  case class Treat() extends Command // treat it as per "the character" entry below.
  case class Start(corefId: String) extends Command //Start a new attribute in the current tag token.

  // Multiply the character reference code by 16
  case class Multiply(obj: String, by: String) extends Command

  // Add a numeric version of the current input character as a hexadecimal digit (subtract 0x0037 from the character's code point) to the character reference code.
  case class Add(obj: String, to: String) extends Command

  case class If(bool: Bool, var T: List[Command], var F: List[Command]) extends Command

  // 仮のやつ
  case class IF_(bool: Bool) extends Command
  case class OTHERWISE_() extends Command

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
  case class CharacterReferenceConsumedAsAttributeVal() extends Bool
  case class IsEqual(a: String, b: String) extends Bool
  case class IsExist(a: String) extends Bool
  case class UNDEF(str: String) extends Bool

  trait ImplementValue
  trait StateVal extends ImplementValue
  case class StateName(state: String) extends StateVal
  case object ReturnState extends StateVal
  case object TemporaryBuffer extends ImplementValue

  case object NewStartTagToken extends ImplementValue
  case object NewEndTagToken extends ImplementValue
  case object NewDOCTYPEToken extends ImplementValue

  case object CurrentTagToken extends ImplementValue
  case object CurrentDOCTYPEToken extends ImplementValue
  case object CurrentAttribute extends ImplementValue
  case object CommentToken extends ImplementValue
  case object EndOfFileToken extends ImplementValue
  case class CharacterToken(chara: String) extends ImplementValue

  case class Variable(variable: String) extends ImplementValue
  case object CurrentInputCharacter extends ImplementValue
  case object NextInputCharacter extends ImplementValue
  case class NameOf(token: ImplementValue) extends ImplementValue
  case class ValueOf(token: ImplementValue) extends ImplementValue
  case class FlagOf(variable: Variable) extends ImplementValue
  case class IChar(char: Char) extends ImplementValue
  case class Mojiretu(string: String) extends ImplementValue
  case class Non(str :String) extends ImplementValue

}
