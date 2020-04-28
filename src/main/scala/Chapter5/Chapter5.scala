package Chapter5

object Chapter5 {
  trait Stream[+A]
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] ❶

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = { ❷
      lazy val head = hd ❸
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))
    
  }

  def main(args: Array[String]): Unit = {
    println("a")
  }
}
