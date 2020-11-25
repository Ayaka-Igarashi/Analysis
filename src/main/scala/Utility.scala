object Utility {
  // U+xxxx => char
  def unicodeToString(unicode: String): Char = {
    if (unicode.matches("U.[0-9A-F][0-9A-F][0-9A-F][0-9A-F]")) {
      val str = unicode.substring(2)
      val hex = Integer.parseInt(str, 16)
      hex.toChar
    }
    else ' ' //
  }

  def unicodeToInt(unicode: String): Int = {
    if (unicode.matches("U.[0-9A-F][0-9A-F][0-9A-F][0-9A-F]")) {
      val str = unicode.substring(2)
      val hex = Integer.parseInt(str, 16)
      hex
    }
    else 0
  }
}
