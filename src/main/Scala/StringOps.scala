object StringOps {
  val lengthOfField = 14

  def getHeader(numOfTimes: Int, str: String = CMCDisplay.headerChar): String = {
    var temp = ""
    for (i <- 1 to numOfTimes) {
      temp += str
    }
    temp
  }

  def formatString(value: String): String = {
    ensureCorrectSizeLeftJustified(value) + "|"
  }

  def ensureCorrectSizeLeftJustified(str: String, size: Int = lengthOfField) = {
    if (str.length() < size) {
      var temp = str
      while (temp.length < size) {
        temp = temp + " "
      }
      temp
    } else if (str.length() > size) {
      removeLengthFromString(str, str.length - size)
    } else {
      str
    }
  }

  def removeLengthFromString(str: String, i: Int): String = {
    str.substring(0, str.length() - i)
  }

  def formatHeaderString(value: Header): String = {
    value.prefix + ensureCorrectSizeCenterJustified(value.name, size = lengthOfField + value.lengthModifier) + value.postfix
  }

  def formatNumber(value: Double, isCurrency: Boolean = true, isEnd: Boolean = false): String = {
    val str = (math rint (value * 100)).toString
    val middleIndex = if (str.length > 3) {
      str.length - 4
    } else {
      1
    }

    var outputStr = str.substring(0, middleIndex) + "." + str.substring(middleIndex, str.length - 2)

    outputStr += (if (outputStr.matches("""[\$0-9]*\.[0-9]""")) {
      "0"
    } else {
      if (outputStr.endsWith(".")) {
        "00"
      } else {
        ""
      }
    })

    if (isCurrency) {
      outputStr = "$" + outputStr
    }
    ensureCorrectSizeRightJustified(outputStr) + (if (!isEnd) {
      "|"
    } else {
      ""
    })
  }

  def ensureCorrectSizeRightJustified(str: String, size: Int = lengthOfField) = {
    if (str.length() < size) {
      addLengthToBackOfString(str, size - str.length())
    } else {
      str + " "
    }
  }

  def ensureCorrectSizeCenterJustified(str: String, size: Int = lengthOfField) = {
    if (str.length() < size) {
      addLengthToFrontAndBackOfString(str, size - str.length)
    } else if (str.length() > size) {
      removeLengthFromString(str, str.length - size)
    } else {
      str
    }
  }

  def addLengthToFrontOfString(str: String, i: Int): String = {
    if (i == 1) {
      " " + str
    } else {
      addLengthToFrontOfString(" " + str, i - 1)
    }
  }

  def addLengthToBackOfString(str: String, i: Int): String = {
    if (i == 1) {
      str + " "
    } else {
      addLengthToFrontOfString(str + " ", i - 1)
    }
  }

  def addLengthToFrontAndBackOfString(str: String, i: Int): String = {
    if (i == 1) {
      " " + str
    } else {
      addLengthToBackAndFrontOfString(" " + str, i - 1)
    }
  }

  def addLengthToBackAndFrontOfString(str: String, i: Int): String = {
    if (i == 1) {
      str + " "
    } else {
      addLengthToFrontAndBackOfString(str + " ", i - 1)
    }
  }
}