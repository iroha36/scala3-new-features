package polymorphic_function_type


object PolymorphicFunctionTypeMain extends App {
  /**
   * see also [[https://docs.scala-lang.org/scala3/reference/new-types/polymorphic-function-types.html]]
   * */
  // A polymorphic method:
  def foo[A](xs: List[A]): List[A] = xs.reverse

  // A polymorphic function value:
  val bar: [A] => List[A] => List[A]
  //       ^^^^^^^^^^^^^^^^^^^^^^^^^
  //       a polymorphic function type
  = [A] => (xs: List[A]) => foo[A](xs)

  enum Expr[A]:
    case Var(name: String)
    case Apply[A, B](fun: Expr[B => A], arg: Expr[B]) extends Expr[A]


  object Expr:
    def mapSubexpressions[A](e: Expr[A])(f: [B] => Expr[B] => Expr[B]): Expr[A] =
      e match
        case Apply(fun, arg) => Apply(f(fun), f(arg))
        case Var(n) => Var(n)

    val e0 = Apply(Var("f"), Var("a"))
    val e1 = mapSubexpressions(e0)(
      [B] => (se: Expr[B]) => Apply(Var[B => B]("wrap"), se))
    def showResult(): Unit = println(e1) // Apply(Apply(Var(wrap),Var(f)),Apply(Var(wrap),Var(a)))


  // ------l-----------------------------------------------------------
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
  val optionToList: [A] => Option[A] => List[A] = [A] => (oa: Option[A]) => oa match {
    case Some(value) => List(value)
    case None => List.empty
  }


  println(optionToList(Some(42))) // List(42)
  println(optionToList(Some("abc"))) // List(42)
  println(optionToList(None)) // List()
  Expr.showResult()
}
