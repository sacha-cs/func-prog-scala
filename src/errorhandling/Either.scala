/*
 * Sacha Cohen-Scali - 2017
 */
package errorhandling

sealed trait Either[+E,+A] {

  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(x)  => Right(f(x))
      case Left(err) => Left(err)
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(x)  => f(x)
      case Left(err) => Left(err)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(x) => Right(x)
      case Left(_)  => b
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      b1 <- b
    } yield f(a, b1)
}

case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {

  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil      => Right(Nil)
      case hd :: tl => f(hd).map2(traverse(tl)(f))(_ :: _)
    }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(identity)

}
