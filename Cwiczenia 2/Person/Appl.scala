object Appl extends App {

  override def main(agrs: Array[String]) {
    val p = new Person("Jan", "Kowalski", "1234567890")
    println(p.getSurname())
    p.setSurname("Maziarz")
    println(p.name)
  }
}