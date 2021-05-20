import java.io.PrintWriter

import Main.nStateList
import StateParsedStructure.nState
import TagStructure.{Tag, displayTag, getFirstLeaf}
import Term.displayTerm

object AUMain {
  var txtOut: PrintWriter = null
  
  def auMain() = {
    txtOut = new PrintWriter("src/output2.txt")
    nStateList = PreserveDefinition.read[List[nState]]("src/parsed.dat")
    var SwitchTagList: List[Tag] = List()
    var SetTagList: List[Tag] = List()
    var EmitTagList: List[Tag] = List()
    var AppendList: List[Tag] = List()
    for (ns <- nStateList) {for (tr <- ns.trans) {for (ta <- tr.process._4) {
      if (getFirstLeaf(ta) == "switch") {SwitchTagList :+= ta}
      if (getFirstLeaf(ta) == "set") {SetTagList :+= ta}
      if (getFirstLeaf(ta) == "emit") {EmitTagList :+= ta}
      if (getFirstLeaf(ta) == "append") {AppendList :+= ta}
    }}}
    SwitchTagList.slice(0,2).foreach(t => txtOut.println(displayTag(t)))
    txtOut.println(displayTerm(AntiUnification.antiUnification(SwitchTagList.slice(0,2))))
    txtOut.println()

    txtOut.println("Set ==" + SetTagList.length +"=======================================")
    AUPair(SetTagList)
    txtOut.println("=============================================")
//    txtOut.println("Append ==" + AppendList.length +"=======================================")
//    AUPair(AppendList)
//    txtOut.println("=============================================")
//    txtOut.println("Emit =========================================")
//    AUPair(EmitTagList)
//    txtOut.println("=============================================")
  }

  def AUPair(list: List[Tag]) = {
    var lst = list
    while (lst.length > 1) {
      val head = lst.head
      val tail = lst.tail
      for (t <- tail) {
        txtOut.println(displayTag(head))
        txtOut.println(displayTag(t))
        txtOut.print(" => ")
        txtOut.println(displayTerm(AntiUnification.antiUnification(List(head, t))))
        txtOut.println()
      }
      lst = tail
    }
  }
}
