/*
 * Sacha Cohen-Scali - 2017
 */
package gettingstarted

object GettingStarted {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n


  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }


  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prev: Int, curr: Int): Int =
      if (n == 0) prev
      else go(n - 1, curr, prev + curr)

    go(n, 0, 1)
  }


  private def formatResult(name: String, n: Int, f: Int => Int): String =
    s"The $name of $n is ${f(n)}"

  def findFirst[T](arr: Array[T], eq: T => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= arr.length) -1
      else if (eq(arr(n))) n
      else loop(n + 1)

    loop(0)
  }


  def isSorted[A](arr: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean = {
      if (n >= arr.length - 1) false
      else if (gt(arr(n+1), arr(n))) true
      else go(n + 1)
    }

    go(0)
  }


  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)


  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)


  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))


  def addString(x: String, y: String): Int =
    Integer.parseInt(x) + Integer.parseInt(y)

  def squared(x: Double): Double =
    Math.pow(x, 2)

  def double(x: Double): Double =
    x * 2

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute", -10, abs))
    println(formatResult("factorial", 10, factorial))
    println(formatResult("fibonacci", 10, fib))

    println(findFirst[Int](Array(3, 1, 5, 10, 7), (x: Int) => x == 10))
    println(isSorted[Int](Array(1, 5, 6, 7, 100), (x: Int, y: Int) => x >= y))


    val f: (String) => (String) => Int = curry[String, String, Int](addString)
    val g: (String, String) => Int = uncurry[String, String, Int](f)

    val y = f("2")("2")
    val z = g("2", "2")
    println(y)
    println(z)


    val doubleSquared: Double => Double = compose[Double, Double, Double](double, squared)
    println(doubleSquared(4))

  }
}
