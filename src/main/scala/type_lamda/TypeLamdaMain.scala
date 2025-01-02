package type_lamda

// ---------------------------------------------------------------------------------------------------
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

// Either の部分適用
type EitherT[E] = [A] =>> Either[E, A]

val eitherFunctor: Functor[EitherT[String]] = new Functor[EitherT[String]] {
  def map[A, B](fa: Either[String, A])(f: A => B): Either[String, B] =
    fa match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }
}

object Main extends App {
  println(eitherFunctor.map(Right(1))(_ + 2))
  println(eitherFunctor.map(Right("3"))(_.toInt * 2))
  println(eitherFunctor.map(Left[String, String]("3"))(_.toInt * 2))
}