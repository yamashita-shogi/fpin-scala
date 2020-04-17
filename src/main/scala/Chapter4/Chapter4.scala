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

    def sequence(a: List[Option[A]]): Option[List[A]] = a match {
      case Nil    => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }
  }

  def main(args: Array[String]): Unit = {
    //println("a")
    //println(Some(1.0), Some(2.0).map(_.toString))

    //println(Option.map2(Some(1), Some(2))((x, y) => x + y))
    println(Option.sequence(List(Some(1), Some(2), Some(3), Some(4))))
  }
}
