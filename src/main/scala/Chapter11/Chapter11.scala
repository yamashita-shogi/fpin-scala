package Chapter11

object Chapter11 {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
      (map(fab)(_._1), map(fab)(_._2))

    def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
      e match {
        case Left(fa)  => map(fa)(Left(_))
        case Right(fb) => map(fb)(Right(_))
      }
  }

  trait Monad[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

    def map[A, B](ma: F[A])(f: A => B): F[B]                    =
      flatMap(ma)(a => unit(f(a)))
    def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))

    def sequence[A](lma: List[F[A]]): F[List[A]] =
      lma.foldRight(unit(List[A]()))((a, acc) => map2(a, acc)(_ :: _))

    def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
      la.foldRight(unit(List[B]()))((a, acc) => map2(f(a), acc)(_ :: _))

    def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
      sequence(List.fill(n)(ma))

    def _replicateM[A](n: Int, ma: F[A]): F[List[A]] =
      map(ma)(a => List.fill(n)(a))

    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
      ms match {
        case Nil    => unit(Nil)
        case h :: t =>
          flatMap(f(h))(b =>
            if (!b) filterM(t)(f)
            else map(filterM(t)(f))(h :: _)
          )
      }

    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
      a => flatMap(f(a))(g)

    def _flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
      compose((_: Unit) => ma, f)(())

    def join[A](mma: F[F[A]]): F[A] =
      flatMap(mma)((a: F[A]) => a)

    def flatMapViaJoin[A, B](ma: F[A])(f: A => F[B]): F[B] =
      join(map(ma)(a => f(a)))

    def _ccompose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
      a => flatMapViaJoin(f(a))(g)

  }

//  ParとParserはやってないしスルー
//  val parMonad = new Monad[Par] {
//    def unit[A](a: => A) = Par.unit(a)
//    def flatMap[A, B](ma: Par[A])(f: A => Par[B]) = Par.flatMap(ma)(f)
//  }
//
//  def parserMonad[P[+ _]](p: Parsers[P]) = new Monad[P] {
//    def unit[A](a: => A) = p.succeed(a)
//    def flatMap[A, B](ma: P[A])(f: A => P[B]) = p.flatMap(ma)(f)
//  }

  val optionMonad                     = new Monad[Option] {
    def unit[A](a: => A)                                = Some(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
  }

  val streamMonad                     = new Monad[Stream] {
    def unit[A](a: => A)                                = Stream(a)
    def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]) = ma flatMap f
  }

  val listMonad                       = new Monad[List] {
    def unit[A](a: => A)                            = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]) = ma flatMap f
  }

  def main(args: Array[String]): Unit =
    println("a")
//    println("main = ", listMonad.filterM(List(1, 2, 3, 4))(x => x))
//    println(listMonad.replicateM(3, List(2)))
//    println(listMonad._replicateM(3, List(2)))

//    println(listMonad.filterM(List(1, 2, 3, 4))(x => List(x % 2 == 0)))
//    println(
//      optionMonad.filterM(List(Some(1), Some(2)))(x => Some(x.get % 2 == 0))
//    )

//    println(listMonad)
}
