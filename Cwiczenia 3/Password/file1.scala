package utils

object PasswdGen {
  val allowedChars = (('0' to '9') ++ ('A' to 'Z') ++ ('a' to 'z')).toArray ++ Array('-', '.', '_')
  val allowedCharLength = allowedChars.length

  def nextChar = allowedChars(util.Random.nextInt(allowedCharLength))

  def nextPasswd(passwdLen: Int): String = {
    val passwd = new StringBuilder(passwdLen)
    for (i <- 0 to passwdLen) {
      passwd += nextChar
    }

    passwd.toString
  }
}