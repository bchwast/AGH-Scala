import utils._

object Appl {
  def readFile(inFileName: String = "logins.txt", outFileName: String = "login-passwds.txt") = try {
    val inFile = scala.io.Source.fromFile(inFileName)
    val outFile = new java.io.PrintWriter(outFileName)
    try {
      for (login <- inFile.getLines) {
        outFile.println(login + ":" + PasswdGen.nextPasswd(10))
      }
    } finally {
      inFile.close
      outFile.close
    }
  } catch {
    case ex: java.io.FileNotFoundException =>
      println(ex.getMessage)
    case ex: Throwable =>
      println("Default exception handler: "+ ex.getMessage)
  }

  def main(args: Array[String]) {
    if (args.length == 2) readFile(args(0), args(1))
    else readFile()
  }
}