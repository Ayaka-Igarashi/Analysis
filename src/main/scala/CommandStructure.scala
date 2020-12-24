// 命令の構造体
object CommandStructure {
  trait Command

  case class Switch(state: IStateVal) extends Command
  case class Reconsume(state: IStateVal) extends Command

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
  case class Set(obj: CommandValue, to: CommandValue) extends Command

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
  case class Emit(token: CommandValue) extends Command

  case class Error(error: String) extends Command

  // Append the lowercase version of the current input character (add 0x0020 to the character's code point) to the current tag token's tag name.
  // Append a U+FFFD REPLACEMENT CHARACTER character to the current tag token's tag name.
  case class Append(obj: CommandValue, to: CommandValue) extends Command

  // create a comment token whose data is the empty string
  case class Create(token: CommandValue, valueKey: String) extends Command

  // Ignore the character.
  case class Ignore(obj: String) extends Command

  // 1パターンしかないから決め打ちする
  case class FlushCodePoint() extends Command // Flush code points consumed as a character reference.
  case class TreatAsAnythingElse() extends Command // treat it as per "the character" entry below.
  case class StartNewAttribute(corefId: String) extends Command //Start a new attribute in the current tag token.

  // Multiply the character reference code by 16
  case class MultiplyBy(obj: CommandValue, by: CommandValue) extends Command

  // Add a numeric version of the current input character as a hexadecimal digit (subtract 0x0037 from the character's code point) to the character reference code.
  case class AddTo(obj: CommandValue, to: ImplementVariable) extends Command

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
  case class CurrentEndTagIsAppropriate() extends Bool
  case class IsEqual(a: CommandValue, b: CommandValue) extends Bool
  case class IsExist(a: String) extends Bool
  case class UNDEF(str: String) extends Bool

  trait CommandValue
  trait IStateVal extends CommandValue
  case class StateName(state: String) extends IStateVal
  case object ReturnState extends IStateVal
  case object TemporaryBuffer extends CommandValue
  case object CharacterReferenceCode extends CommandValue

  case object NewStartTagToken extends CommandValue
  case object NewEndTagToken extends CommandValue
  case object NewDOCTYPEToken extends CommandValue
  case object NewCommentToken extends CommandValue

  case object CurrentTagToken extends CommandValue
  case object CurrentDOCTYPEToken extends CommandValue
  case object CurrentAttribute extends CommandValue
  case object CommentToken extends CommandValue
  case object EndOfFileToken extends CommandValue
  case class CharacterToken(chara: String) extends CommandValue

  case class Variable(variable: String) extends CommandValue
  case object CurrentInputCharacter extends CommandValue
  case object NextInputCharacter extends CommandValue//
  case class NameOf(token: CommandValue) extends CommandValue//いらない
  case class ValueOf(token: CommandValue) extends CommandValue//いらない
  case class LowerCase(token: CommandValue) extends CommandValue
  case class NumericVersion(token: CommandValue) extends CommandValue
  case class FlagOf(variable: Variable) extends CommandValue//いらない
  case class CChar(char: Char) extends CommandValue
  case class CString(string: String) extends CommandValue
  case class CInt(int: Int) extends CommandValue
  case class Non(str :String) extends CommandValue//


  // 代入される変数
  trait ImplementVariable
  case object IReturnState extends ImplementVariable
  case object ITemporaryBuffer extends ImplementVariable
  case object ICharacterReferenceCode extends ImplementVariable
  case object ICurrentTagToken extends ImplementVariable
  case object ICurrentDOCTYPEToken extends ImplementVariable
  case object ICurrentAttribute extends ImplementVariable
  case object ICommentToken extends ImplementVariable
  case class IVariable(variable: String) extends ImplementVariable
  case class INameOf(token: ImplementVariable) extends ImplementVariable
  case class IValueOf(token: ImplementVariable) extends ImplementVariable
  case class IFlagOf(token: ImplementVariable) extends ImplementVariable

}
