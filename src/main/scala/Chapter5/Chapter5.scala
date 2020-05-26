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

    // 自作
    //    def headOption_1: Stream[A] =
    //      foldRight(Stream.empty[A])((h, _) => Stream.cons(h, Stream.empty))

    def headOption: Option[A] =
      foldRight(None: Option[A])((h, _) => Some(h))

    def map[B](f: A => B): Stream[B] =
      foldRight(Stream.empty[B])((h, t) => Stream.cons(f(h), t))

    def filter(f: A => Boolean): Stream[A] =
      foldRight(Stream.empty[A])(
        (h, t) =>
          if (f(h)) Stream.cons(h, t)
          else t
      )

    def append[B >: A](s: => Stream[B]): Stream[B] =
      foldRight(s)((h, t) => Stream.cons(h, t))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(Stream.empty[B])((h, t) => f(h) append t)

//    def map_u[A, B](f: A => B): Stream[B] = this match {
//      case Cons(h, t) => Stream.unfold(h)(_ => Some(f(h()), t))
//      case _          => Stream.empty
//    }

    def mapViaUnfold[B](f: A => B): Stream[B] =
      Stream.unfold(this) {
        case Cons(h, t) => Some((f(h()), t()))
        case _          => None
      }

    def take_u(n: Int): Stream[A] = {
      Stream.unfold(this) {
        case Cons(h, t) if n > 1  => Some(h(), t().take_u(n - 1))
        case Cons(h, _) if n == 1 => Some(h(), Stream.empty)
        case _                    => None
      }
    }

    def takeViaUnfold(n: Int): Stream[A] =
      Stream.unfold((this, n)) {
        case (Cons(h, t), 1)          => Some((h(), (Stream.empty, 0)))
        case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
        case _                        => None
      }

    def takeWhile_u(f: A => Boolean): Stream[A] = {
      Stream.unfold(this) {
        case Cons(h, t) => if (f(h())) Some(h(), t()) else None
      }
    }

    def zipWith_u[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
      Stream.unfold((this, s2)) {
        case (Cons(h1, t1), Cons(h2, t2)) =>
          Some((f(h1(), h2()), (t1(), t2())))
        case _ => None
      }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
      zipWithAll(s2)((_, _))

    def zipWithAll[B, C](
      s2: Stream[B]
    )(f: (Option[A], Option[B]) => C): Stream[C] =
      Stream.unfold((this, s2)) {
        case (Empty, Empty) => None
        case (Cons(h, t), Empty) =>
          Some(f(Some(h()), Option.empty[B]) -> (t(), Stream.empty[B]))
        case (Empty, Cons(h, t)) =>
          Some(f(Option.empty[A], Some(h())) -> (Stream.empty[A] -> t()))
        case (Cons(h1, t1), Cons(h2, t2)) =>
          Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
      }

    // 5.14
//    def startsWith[A](s: Stream[A]): Boolean = {
//      println(this.toList, s.toList)
//      (this, s) match {
//        case (Cons(_, _), Empty)                      => true
//        case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => t1().startsWith(t2())
//        case _                                        => false
//      }
//    }

    def startsWith[A](s: Stream[A]): Boolean =
      zipAll(s).takeWhile(!_._2.isEmpty) forAll {
        case (h, h2) => h == h2
      }
//
    def tails: Stream[Stream[A]] =
      Stream.unfold(this) {
        case Cons(h, t) => Some(Stream.cons(h(), t()), t())
        case _          => None
      } append Stream(Stream.empty)

    // 模範
//    def tails: Stream[Stream[A]] =
//      Stream.unfold(this) {
//        case Empty => None
//        case s     => Some((s, s drop 1))
//      } append Stream(Stream.empty)

//    def scanRight[A, B, C](x: A)(f: (A, B) => C): Stream[Stream[C]] =
//      Stream.unfold(this) {
//        case Cons(h, t) => Some(h(), t().tails)
//        case _          => Stream.empty[A]
//      }
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

    val ones: Stream[Int] = Stream.cons(1, ones)

    def constant[A](a: A): Stream[A] =
      Stream.cons(a, constant(a))

    def from(n: Int): Stream[Int] =
      Stream.cons(n, from(n + 1))

    val fibs = {
      def go(f0: Int, f1: Int): Stream[Int] =
        Stream.cons(f0, go(f1, f0 + f1))

      go(0, 1)
    }

    //    def unfold1[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    //      f(z) match {
    //        case (Some[(a, _)]) => Stream.cons(a, unfold(z)(f))
    //        case (_, s) => Stream.cons(z, s)
    //      }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case Some((h, s)) => cons(h, unfold(s)(f))
        case None         => empty
      }

    def unfold_1[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(z) match {
        case Some(t) => cons(t._1, unfold_1(t._2)(f))
        case None    => Empty
      }
    }

    val ones_U: Stream[Int] =
//      Stream.unfold(1)(s => if (s != 1) None else Some(s, s))
      Stream.unfold(1)(s => Some(s, s))

    def constant_U[A](a: A): Stream[A] =
      unfold(a)(s => Some(s, s))

    def from_U(a: Int): Stream[Int] =
      unfold(a)(s => Some(s, s + 1))

//    val fibs = {
//      def go(f0: Int, f1: Int): Stream[Int] =
//      //Stream.cons(f0, go(f1, f0 + f1))
//        Stream.unfold(f0)(s => Some(go(f1, f0 + f1)))
//
//      go(0, 1)
//    }
//
//    def fibs_U(): Stream[Int] =
//      Stream.unfold(fibs_(0))(s => Some(fibs_(s), fibs_(fibs_(s) + 1)))
//
//    private def fibs_(n: Int): Int = n match {
//      case 0 => 0
//      case 1 => 1
//      case 2 => 1
//      case x => fibs_(x - 1) + fibs_(x - 2)
//    }
    val fibsViaUnfold =
      unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

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

//    // exercise 5.5
//    val s5 = Stream(2, 4, 5, 8)
//    println("takeWhile_1=" + s5.takeWhile_1(_ % 2 == 0).toList)

//    // exercise 5.6
//    val s6 = Stream(4, 5, 8)
//    //println("forAll=" + s6.headOption_1.toList)
//    println("headOption=" + s6.headOption)

//    // exercise 5.7
//    val s7 = Stream("1", "2", "3", "4")
//    //println("forAll=" + s6.headOption_1.toList)
//    println("map=" + s7.map(_.toInt).toList)

    //println(Stream.ones.map(_ + 1).exists(_ % 2 == 0))

//    // exersice 5.9
//    println(Stream.from(1).take(10).toList)

//    // exersice 5.10
//    println(Stream.fibs.take(10).toList)

//    // exersice 5.11
//    println(
//      Stream
//        .unfold(10)(s => Some(s, s - 1))
//        .take(5)
//        .toList
//    )

    // exersice 5.12
//    println(Stream.ones_U.take(5).toList)
//    println(Stream.constant_U("a").take(5).toList)
//    println(Stream.from_U(10).take(5).toList)
//    println(Stream.fibsViaUnfold.take(5).toList)

    // exercise 5.13
//    val s13_1 = Stream("1", "2", "3", "4", "5")
//    println("mapViaUnfold=" + s13_1.mapViaUnfold(_.toInt).toList)

//    val s13_2 = Stream(1, 2, 3, 4, 5)
//    println("take_u=" + s13_2.take_u(3).toList)

//    val s13_3 = Stream(2, 4, 5, 8)
//    println("takeWhile_u=" + s13_3.takeWhile_u(_ % 2 == 0).toList)

//    val s13_4 = Stream(1, 1, 1)
//    println(s13_4.zipWith_u(Stream(2, 2, 2))(_ + _).toList)

//    val s13_5 = Stream(1, 1, 1)
//    println("zipAll" + s13_5.zipAll(Stream(2, 2)).toList)

//    // exercise 5.14
//    val s14 = Stream(1, 2, 3)
//    println(s14.startsWith(Stream(1, 2)))

//    // exercise 5.15
//    val s15 = Stream(1, 2, 3)
//    println(s15.tails.toList.foreach(x => println(x.toList)))

    println(List(1, 2, 3).scanRight(100)((n, z) => n - z))
  }
}
