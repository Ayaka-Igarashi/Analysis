object CharacterMatching {
  // character matching
  def isNonCharacter(c: Long): Boolean = {
    if (0xfdd0 <= c && c <= 0xfdef) true
    else {
      c match {
        case 0xFFFE|0xFFFF|0x1FFFE|0x1FFFF|0x2FFFE|0x2FFFF|0x3FFFE|0x3FFFF|0x4FFFE|0x4FFFF|0x5FFFE|0x5FFFF|0x6FFFE|0x6FFFF|0x7FFFE|0x7FFFF|0x8FFFE|0x8FFFF|0x9FFFE|0x9FFFF|0xAFFFE|0xAFFFF|0xBFFFE|0xBFFFF|0xCFFFE|0xCFFFF|0xDFFFE|0xDFFFF|0xEFFFE|0xEFFFF|0xFFFFE|0xFFFFF|0x10FFFE|0x10FFFF => {
          true
        }
        case _ => false
      }
    }
  }

  def isC0Control(c: Long): Boolean = {
    0x0000 <= c && c <= 0x001F
  }

  def isControl(c: Long): Boolean = {
    isC0Control(c) || (0x007F <= c && c <= 0x009F)
  }

  def isASCIIWhitespace(c: Long): Boolean = {
    c match {
      case 0x0009|0x000A|0x000C|0x000D|0x0020 => true
      case _ => false
    }
  }

  def isASCIIDigit(c: Long): Boolean = {
    0x0030 <= c && c <= 0x0039
  }

  def isASCIIUpperAlpha(c: Long): Boolean = {
    0x0041 <= c && c <= 0x005A
  }

  def isASCIILowerAlpha(c: Long): Boolean = {
    0x0061 <= c && c <= 0x007A
  }

  def isASCIIAlpha(c: Long): Boolean = {
    isASCIIUpperAlpha(c) || isASCIILowerAlpha(c)
  }

  def isASCIIAlphaNumeric(c: Long): Boolean = {
    isASCIIDigit(c) || isASCIIAlpha(c)
  }
}
