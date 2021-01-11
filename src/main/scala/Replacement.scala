import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

object Replacement {
  // 置き換え結果出力
  val replace_out = new PrintWriter(new BufferedWriter(new FileWriter(new File("src/replaceOut.txt"))))
  var replaceState: String = "[mM]arkup declaration open state|[nN]amed character reference state|[nN]umeric character reference end state"

  // 自然言語処理しやすいように、文字の置き換えを行う
  def replace(str: String): String = {
    var s = str
    replace_out.println(s)

    // 状態名を1つのtokenにする
    val re1 = replaceState.r
    s = re1.replaceAllIn(s, m => {
      val m2 = m.toString().replace(" ", "_").replace("-","_")
        .replace("(", "").replace(")", "")
      m2(0).toUpper + m2.tail
    })

    s = s.replace("-", "_")

    // U+xxxx => UPxxxx
    val re2 =  "(U\\+[0-9A-F][0-9A-F][0-9A-F][0-9A-F])".r
    s = re2.replaceAllIn(s, m => "U_" + m.toString().substring(2,6))
    // switch => you switch
    val re3 = "([sS]witch|[rR]econsume|[eE]mit|[fF]lush|[aA]ppend|[aA]dd|[mM]ultiply)".r//
    s = re3.replaceAllIn(s, m => "you " + m.toString())
//    s = s.replace("Multiply", "multiply")
    // ! => -EXC-
    val re4 = "(\\!)".r
    s = re4.replaceAllIn(s, m => "EXC")

    // 参照関係
    // ",", "." => " and"
    val re5 = "(\\.|\\,) [sS]et".r
    s = re5.replaceAllIn(s, m => " and" + m.toString().tail)
//    s = s.replace("and and", "and")
//    s = s.replace("and then", ", then")
//    s = s.replace("and Otherwise and", ". Otherwise,")
    s = s.replace("error and", "error.")

    s = s.replace("that attribute", "that attribute's")
    s = s.replace("'s's", "'s")
    s = ":$".r.replaceAllIn(s, ".")

    replace_out.println(" => " + s)
    s
  }
}
