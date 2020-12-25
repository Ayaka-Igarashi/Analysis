import CommandStructure._
import StateProcessedStructure.pState

object OtherStates {
  val  Markup_declaration_open_state =
    pState("Markup_declaration_open_state",
      List(If(UNDEF("Match(next characters, IString(--))"), List(Consume("--"), Create(NewCommentToken, ""), Switch(StateName("Comment_start_state"))), List(
            If(UNDEF("DOCTYPE"), List(Consume("DOCTYPE"), Switch(StateName("DOCTYPE_state"))), List(
              If(UNDEF("[CDATA["), List(Consume("[CDATA["),
                      If(And(IsExist(""), Not(IsEqual(Non(""), Non("")))), List(Switch(StateName("CDATA_section_state"))), List(Error("cdata_in_html_content parse error"), Create(NewCommentToken, "x_1"), Set(IValueOf(IVariable("x_1")), CString("[CDATA[")), Switch(StateName("Bogus_comment_state"))))),
                List(Error("incorrectly_opened_comment parse error"), Create(NewCommentToken, ""), Switch(StateName("Bogus_comment_state")))
            ))
      )))
      ),
      List())

  val  Named_character_reference_state =
    pState("Named_character_reference_state",
      List(),
      List()
    )

  val  Numeric_character_reference_end_state =
    pState("Numeric_character_reference_end_state",
      List(),
      List()
    )

}
