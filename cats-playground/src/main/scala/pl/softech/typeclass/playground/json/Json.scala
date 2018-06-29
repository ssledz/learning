package pl.softech.typeclass.playground.json

sealed trait Json

final case class JsObject(get: Map[String, Json]) extends Json

final case class JsString(get: String) extends Json

final case class JsNumber(get: Double) extends Json

case object JsNull extends Json

trait JsonWriter[A] {

  def write(value: A): Json

}

case class Person(name: String, email: String, age: Option[Int])

object JsonWriterInstances {

  implicit val stringWriter: JsonWriter[String] = (value: String) => JsString(value)
  implicit val integerWriter: JsonWriter[Int] = (value: Int) => JsNumber(value)
  implicit val personWriter: JsonWriter[Person] = (value: Person) =>
    JsObject(Map(
      "name" -> Json.toJson(value.name),
      "email" -> Json.toJson(value.email),
      "age" -> Json.toJson(value.age))
    )

  implicit def optionWriter[A](implicit w: JsonWriter[A]): JsonWriter[Option[A]] =
    new JsonWriter[Option[A]] {
      override def write(value: Option[A]): Json = value match {
        case Some(value) => w.write(value)
        case None => JsNull
      }
    }


}

object Json {

  def toJson[A](value: A)(implicit writer: JsonWriter[A]): Json = {
    writer.write(value)
  }

}

object JsonSyntax {

  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json = Json.toJson(value)

  }

}

object Test extends App {

  import JsonSyntax._
  import JsonWriterInstances._

  val tom = Person("Tom", "tom@wp.pl", Option(11))

  println(Json.toJson(tom))
  println(tom.toJson)
  println(implicitly[JsonWriter[Person]].write(tom))

  println(Option(tom).toJson)
  println(Option[Person](null).toJson)

  println(Person("Jerry", "jerry@wp.pl", None).toJson)
}

