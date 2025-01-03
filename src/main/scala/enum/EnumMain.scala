package `enum`

enum Option[+T]:
  case Some(x: T)
  case None

  def isDefined: Boolean = this match
    case None => false
    case _ => true

object Option:
  def apply[T >: Null](x: T): Option[T] =
    if x == null then None else Some(x)

end Option
class EnumMain {
  Option(1).isDefined
}

