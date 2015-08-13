class Chapter2 {
  def fib(n: Int): Int = {

    def loop(a: Int, b: Int, index: Int): Int = {
      if (index >= n) {
        a + b
      } else {
        loop(b, a + b, index + 1)
      }
    }

    loop(0, 1, 3)

  }

  def isSorted[A](as: Array[A], isOrdered: (A, A) => Boolean): Boolean = {
    as match {
      case x if x.length <= 1 => true
      case x if (isOrdered(x(0), x(1)) == false) => false
      case x => isSorted(x.drop(1), isOrdered)
    }
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => f(a, _)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

}