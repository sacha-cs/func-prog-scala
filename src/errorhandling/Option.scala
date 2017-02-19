/*
 * Sacha Cohen-Scali - 2017
 */
package errorhandling

/*
  +A means that type A is a covariant parameter of Option
  This means we can write things like this:
    val none1: Option[Double] = None
    val none2: Option[String] = None
  This is because Nothing is a subtype of all types
 */
sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] =
    this match {
      case Some(x) => Some(f(x))
      case None    => None
    }

  // => means that default is lazy
  def getOrElse[B>:A](default: => B): B =
    this match {
      case Some(x) => x
      case None    => default
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  // B>:A means B is a super type of A
  def orElse[B>:A](ob: => Option[B]): Option[B] =
    this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    this match {
      case Some(x) if f(x) => this
      case _               => None
    }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(x), Some(y)) => Some(f(x, y))
      case (_, _)             => None
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil      => Some(Nil)
      case hd :: tl => hd.flatMap(h => sequence(tl).map(h :: _))
    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil      => Some(Nil)
      case hd :: tl => map2(f(hd), traverse(tl)(f))(_ :: _)
    }

  def sequenceWithTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)
}
