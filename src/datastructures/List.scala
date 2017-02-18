/*
 * Copyright (c) 2016 Schibsted Media Group. All rights reserved
 */
package datastructures

/*
  +A means that type A is a covariant parameter of List
  This means we can write things like this:
    val nil1: List[Double] = Nil
    val nil2: List[String] = Nil
  This is because Nothing is a subtype of all types
 */
sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*)) // _* means pass each element as its own argument, rather than one single Seq argument

  def tail[A](list: List[A]): List[A] =
    list match {
      case Nil         => throw new UnsupportedOperationException("Cannot get the tail of empty List")
      case Cons(_, tl) => tl
    }

  def setHead[A](list: List[A], newHead: A): List[A] =
    list match {
      case Nil         => throw new UnsupportedOperationException("Cannot set the head of an empty List")
      case Cons(_, tl) => Cons(newHead, tl)
    }

  def drop[A](list: List[A], n: Int): List[A] = {
    require(n > 0)
    list match {
      case Nil => Nil
      case Cons(hd, tl) => drop(tl, n - 1)
    }
  }

  /*
    Make the function curried so we can use type inference and call the function like this:
      val list = List(1,2,3,4,5)
      dropWhile(list)(_ < 4)
    instead of
      dropWhile(list, (x: Int) => x < 4)
  */
  def dropWhile[A](list: List[A])(f: A => Boolean): List[A] =
    list match {
      case Cons(hd, tl) if f(hd) => dropWhile(tl)(f)
      case _                     => list
    }

  def init[A](list: List[A]): List[A] =
    list match {
      case Nil          => throw new UnsupportedOperationException("Cannot init an empty List")
      case Cons(_, Nil) => Nil
      case Cons(hd, tl) => Cons(hd, init(tl))
    }

  def sum(ints: List[Int]): Int = {
    ints match {
      case Nil         => 0
      case Cons(x, xs) => x + sum(xs)
    }
  }

  def product(doubles: List[Double]): Double = {
    doubles match {
      case Nil         => 1.0
      case Cons(x, xs) => x * product(xs)
    }
  }

  def foldRight[A,B](list: List[A], z: B)(f: (A, B) => B): B =
    list match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z), f)
    }

  def sum2(ints: List[Int]): Int =
    foldRight(ints, 0)(_ + _)

  def product2(doubles: List[Double]): Double =
    foldRight(doubles, 1.0)(_ * _)

  def length[A](list: List[A]): Int =
    foldRight(list, 0)((_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](list: List[A], z: B)(f: (B, A) => B): B =
    list match {
      case Nil => z
      case Cons(hd, tl) => foldLeft(tl, f(z, hd))(f)
    }

  def sum3(ints: List[Int]): Int =
    foldLeft(ints, 0)(_ + _)

  def product3(doubles: List[Double]): Double =
    foldLeft(doubles, 1.0)(_ * _)

  def length2[A](list: List[A]): Int =
    foldLeft(list, 0)((acc, _) => acc + 1)

  def reverse[A](list: List[A]): List[A] =
    foldLeft(list, List[A]())((acc, hd) => Cons(hd, acc))

  def foldRightWithFoldLeft[A,B](list: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(list), z)((b,a) => f(a,b))

  def foldLeftWithFoldRight[A,B](list: List[A], z: B)(f: (B,A) => B): B =
    foldRight(list, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  def append[A](list: List[A], more: List[A]): List[A] =
    foldRight(list, more)(Cons(_, _))

  def flatten[A](list: List[List[A]]): List[A] =
    foldRight(list, Nil: List[A])(append)

  def add1(list: List[Int]): List[Int] =
    foldRight(list, Nil: List[Int])((hd, tl) => Cons(hd + 1, tl))

  def toString(list: List[Double]): List[String] =
    foldRight(list, Nil: List[String])((hd, tl) => Cons(hd.toString, tl))

  def map[A,B](list: List[A])(f: A => B): List[B] =
    foldRight(list, Nil: List[B])((h,t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((hd, tl) => if (f(hd)) Cons(hd, tl) else tl)

  def flatMap[A,B](list: List[A])(f: A => List[B]): List[B] =
    flatten(map(list)(f))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def addByPair(as: List[Int], bs: List[Int]): List[Int] =
    (as, bs) match {
      case (Nil, _) | (_, Nil)        => Nil
      case (Cons(hd1, tl1), Cons(hd2, tl2)) => Cons(hd1 + hd2, addByPair(tl1,tl2))
    }

  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] =
    (as, bs) match {
      case (Nil, _) | (_, Nil) => Nil
      case (Cons(hd1, tl1), Cons(hd2, tl2)) => Cons(f(hd1, hd2), zipWith(tl1, tl2)(f))
    }

  @annotation.tailrec
  def prefixedWith[A](list: List[A], prefix: List[A]): Boolean =
    (list, prefix) match {
      case (_, Nil)                                    => true
      case (Cons(hd, tl), Cons(hd2, tl2)) if hd == hd2 => prefixedWith(tl, tl2)
      case _                                           => false
    }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case Nil                         => sub == Nil
      case _ if prefixedWith(sup, sub) => true
      case Cons(hd, tl)                => hasSubsequence(tl, sub)
    }
}

