package Chapter5

object Chapter5 {
  trait Stream[+A] {
    // exercise 5.1
    def toList: List[A] = this match {
      case Cons(h, t) => h() :: t().toList
      case _          => List()
    }

//    def toList: List[A] = {
//      @annotation.tailrec
//      def go(s: Stream[A], acc: List[A]): List[A] = s match {
//        case Cons(h, t) => go(t(), h() :: acc)
//        case _          => acc
//      }
//      go(this, List()).reverse
//    }

    // exercise 5.2
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1  => Stream.cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
      case _                    => Stream.empty
    }

//    @annotation.tailrec
//    final def drop(n: Int): Stream[A] = this match {
//      case Cons(_, t) if n > 0 => t().drop(n - 1)
//      case _                   => this
//    }

    def drop(n: Int): Stream[A] =
      if (n <= 0) this
      else
        this match {
          case Cons(_, t) => t().drop(n - 1)
          //case _          => this
        }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
      case _                    => Empty
    }

//    def takeWhile(p: A => Boolean): Stream[A] = {
//      this match {
//        case Empty => Empty
//        case Cons(h, t) =>
//          if (p(h())) {
//            Stream.cons(h(), t().takeWhile(p))
//          } else {
//            Empty
//          }
//      }
//    }
    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _          => z
      }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

//    def forAll(p: A => Boolean): Boolean =
//      foldRight(false)((a, b) => p(a) || b)

    def forAll(f: A => Boolean): Boolean =
      foldRight(true)((a, b) => f(a) && b)

//    def takeWhile_fr(f: A => Stream[A]): Stream[A] =
//      foldRight(Empty)((a, b) => f(a), Empty)
    def takeWhile_1(f: A => Boolean): Stream[A] =
      foldRight(Stream.empty[A])(
        (h, t) =>
          if (f(h)) Stream.cons(h, t)
          else Stream.empty
      )
  }
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    val s = Stream(1, 2, 3, 4)

//    // exercise 5.1
//    println(s.toList)

    //// exercise 5.2
    //println(s.take(3).toList)

    //println(s.drop(2).toList)
    //println(s.dropOrg(3).toList)
//    (0 to 4).foreach { i =>
//      println(i, s.dropOrg(i).toList)
//    }

//    // exercise 5.3
//    val s3 = Stream(2, 4, 5, 6)
//    println("takeWhile=" + s3.takeWhile(_ % 2 == 0).toList)

//    // exercise 5.4
//    val s4 = Stream(2, 4, 5, 8)
//    println("forAll=" + s4.forAll(_ % 2 == 0))

    // exercise 5.5
    val s5 = Stream(2, 4, 5, 8)
    println("forAll=" + s5.takeWhile_1(_ % 2 == 0).toList)

  }
}
