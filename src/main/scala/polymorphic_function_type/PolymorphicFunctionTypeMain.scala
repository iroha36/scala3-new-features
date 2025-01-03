package polymorphic_function_type

object PolymorphicFunctionTypeMain extends App {
  val toStringFunc: [A] => A => String = [A] => (a: A) => a.toString

  println(toStringFunc(123)) // "123"
  println(toStringFunc(3.14)) // "3.14"
  println(toStringFunc("Scala")) // "Scala"
  // -----------------------------------------------------------------
  val mapList: [A, B] => (List[A], A => B) => List[B] =
    [A, B] => (list: List[A], f: A => B) => list.map(f)

  val numbers = List(1, 2, 3)
  println(mapList(numbers, (x: Int) => x * 2)) // List(2, 4, 6)

  val strings = List("1", "2", "3")
  println(mapList(strings, (x: String) => x.toInt * 3)) // List(3, 6, 9)

  // -----------------------------------------------------------------
  val optionToList: [A] => Option[A] => List[A] = [A] => (oa: Option[A]) => oa match{
    case Some(value) => List(value)
    case None => List.empty
  }

  def f[A](oa: Option[A]): List[A] = oa match {
    case Some(value) => List(value)
    case None => List.empty
  }

  println(optionToList(Some(42))) // List(42)
  println(optionToList(Some("abc"))) // List(42)
  println(optionToList(None)) // List()

}
