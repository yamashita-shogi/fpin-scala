package Chapter4

object Chapter4 {

  sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = this match {
      case None    => None
      case Some(a) => Some(f(a))
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None    => default
      case Some(a) => a
    }

    def flatMap[B](f: A => Option[B]): Option[B] =
      map(f) getOrElse None

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None    => ob
      case Some(x) => Some(x)
    }

//    def filter(f: A => Boolean): Option[A] = this match {
//      case None           => None
//      case Some(a) if (f) => this
//    }

  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  object Option {
    def failingFn(i: Int): Int = {
      // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
      val y: Int = throw new Exception("fail!")
      try {
        val x = 42 + 5
        x + y
      }
      // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern
      // that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
      catch {
        case e: Exception => 43
      }
    }

    def failingFn2(i: Int): Int = {
      try {
        val x = 42 + 5
        // A thrown Exception can be given any type; here we're annotating it with the type `Int`
        x + ((throw new Exception("fail!")): Int)
      } catch {
        case e: Exception => 43
      }
    }

    //  def mean(xs: Seq[Double]): Double =
    //    if (xs.isEmpty)
    //      throw new ArithmeticException("mean of empty list!")
    //    else xs.sum / xs.length

    def mean_1(xs: IndexedSeq[Double], onEmpty: Double): Double =
      if (xs.isEmpty) onEmpty
      else xs.sum / xs.length

    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] =
      mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      a.flatMap(ax => b.map(bx => f(ax, bx)))

//    def sequence2[A](a: List[Option[A]]): Option[List[A]] =
//      a.foldRight()

    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil    => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
      a match {
        case Nil    => Some(Nil)
        case h :: t => f(h).flatMap(hh => traverse(t)(f) map (hh :: _))
      }
  }

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] =
      this match {
        case Left(a)  => Left(a)
        case Right(a) => Right(f(a))
      }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
      this match {
        case Left(a)  => Left(a)
        case Right(a) => f(a)
      }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
      this match {
        case Left(_)  => b
        case Right(a) => Right(a)
      }

    def map2_1[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      this.flatMap(tt => b.map(bb => f(tt, bb)))

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
        a <- this;
        b1 <- b
      } yield f(a, b1)

  }

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  object Either {
    def mean(xs: IndexedSeq[Double]): Either[String, Double] =
      if (xs.isEmpty)
        Left("mean of empty list!")
      else
        Right(xs.sum / xs.length)

    def safeDiv(x: Int, y: Int): Either[Exception, Int] =
      try Right(x / y)
      catch { case e: Exception => Left(e) }

    def Try[A](a: => A): Either[Exception, A] =
      try Right(a)
      catch { case e: Exception => Left(e) }

    def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      es match {
        case Nil => Right(Nil)
        case f(h).map2(traverse(t)(f))(_ :: _)
        // map2の次のtraverseの呼び方が模範はメソッド、自分が作ったのは
      }

    // 模範
    def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
      traverse(es)(x => x)
  }

  def main(args: Array[String]): Unit = {
    //println("a")
    //println(Some(1.0), Some(2.0).map(_.toString))

    //println(Option.map2(Some(1), Some(2))((x, y) => x + y))
    println(Option.sequence(List(Some(1), Some(2), Some(3), Some(4))))
  }
}
