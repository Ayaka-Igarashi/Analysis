object Replacement {
  // 自然言語処理しやすいように、文字の置き換えを行う
  def replace(str: String): String = {
    var s = str
    println(s)

    // U+xxxx => UPxxxx
    val re =  "(U\\+[0-9A-F][0-9A-F][0-9A-F][0-9A-F])".r
    s = re.replaceAllIn(s, m => "UP" + m.toString().substring(2,6))
    // switch => you switch
    val re2 = "([sS]witch|[rR]econsume|[eE]mit|[fF]lush|[aA]ppend)".r
    s = re2.replaceAllIn(s, m => "you " + m.toString())
    s = s.replace("Multiply", "multiply")
    // ! => -EXC-
    val re3 = "(\\!)".r
    s = re3.replaceAllIn(s, m => "-EXC-")
    // ",", "." => " and"
    val re4 = "(\\.|\\,) [sS]et".r
    s = re4.replaceAllIn(s, m => " and" + m.toString().tail)
    s = s.replace("and and", "and")
    s = s.replace("and then", ", then")
    s = s.replace("and Otherwise and", ". Otherwise,")
    s = s.replace("error and", "error.")

    println(" => " + s)
    s
  }
}
