object Utility {
  // U+xxxx => char
  def unicodeToString(unicode: String): Char = {
    val str = unicode.substring(2)
    val hex = Integer.parseInt(str, 16)
    hex.toChar
  }
}
