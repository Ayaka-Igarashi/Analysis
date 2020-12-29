object Utility {
  // U+xxxx => char
  def unicodeToChar(unicode: String): Char = {
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

  def longIntToCharacter(unicode: Long): String = {
    if (unicode < 0x10000) {
      unicode.toChar.toString
    }
    else {
      ((unicode - 0x10000) / 0x400 + 0xD800).toChar.toString + ((unicode - 0x10000) % 0x400 + 0xDC00).toChar.toString
    }
  }
}
