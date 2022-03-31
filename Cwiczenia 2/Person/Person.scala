import scala.beans.BeanProperty

class Person(val givenName: String, @BeanProperty var surname: String, val id: String) {

  def name = givenName + " " + surname
}