package Chapter11

import scala.::

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

    def map[A, B](ma: F[A])(f: A => B): F[B] =
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
        case Nil => unit(Nil)
        case h :: t =>
          flatMap(f(h))(
            b =>
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

    def _compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
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

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A) = Some(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A) = Stream(a)
    def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]) = ma flatMap f
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A) = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]) = ma flatMap f
  }

  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] = Id(f(value))

    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }

  object Id {
    val idMonad = new Monad[Id] {
      def unit[A](a: => A) = Id(a)
      def flatMap[A, B](ida: Id[A])(f: A => Id[B]): Id[B] = ida flatMap f
    }
  }

  case class State[S, A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      State { s =>
        val (a, s1) = run(s)
        (f(a), s1)
      }
    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State { s =>
        val (a, s1) = run(s)
        f(a).run(s1)
      }
  }

  type IntState[A] = State[Int, A]

  object IntStateMonad extends Monad[IntState] {
    def unit[A](a: => A): IntState[A] = State(s => (a, s))
    def flatMap[A, B](st: IntState[A])(f: A => IntState[B]): IntState[B] =
      st flatMap f
  }

  def stateMonad[S] =
    new Monad[({ type f[x] = State[S, x] })#f] {
      def unit[A](a: => A): State[S, A] =
        State(s => (a, s))

      def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
        st flatMap f
    }

  def getState[S]: State[S, S] = State(s => (s, s))
  def setState[S](s: S): State[S, Unit] = State(_ => ((), s))
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  val F = stateMonad[Int]
  def a[A]: State[Int, List[(Int, A)]] = F.unit(List[(Int, A)]())
  def zipWithIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(F.unit(List[(Int, A)]()))(
        (acc, a) =>
          for {
            xs <- acc
//            _ = println(s"acc: $acc")
//            _ = println(s"xs: $xs")
            n <- getState
            _ <- setState(n + 1)
//            _ = println(s"n: $n")
          } yield (n, a) :: xs
      )
      .run(0)
      ._1
      .reverse

//  def _zipWithIndex[A](as: List[A]): List[(Int, A)] =
//    as.foldLeft(F.unit(List[(Int, A)]()))(
//        (acc, a) =>
//          acc.flatMap(xs => {
//            getState.flatMap(n => {
//              setState(n + 1).map((n, a) :: xs)
//            })
//          })
//      )
//      .run(0)

  def zipWithIndex2[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(F.unit(List[(Int, A)]()))(
        (acc: State[Int, List[(Int, A)]], a: A) => {
          acc.flatMap((xs: Seq[(Int, A)]) => {
            getState.flatMap((n: Int) => {
              setState(n + 1).map((_: Unit) => List((n, a)) ++ xs)
            })
          })
        })
      .run(0)
      ._1
      .reverse

  case class Reader[R, A](run: R => A)
  object Reader {
    def readerMonad[R] =
      new Monad[({ type f[x] = Reader[R, x] })#f] {
        def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

        override def flatMap[A, B](
            st: Reader[R, A]
        )(f: A => Reader[R, B]): Reader[R, B] =
          Reader { r =>
            f(st.run(r)).run(r)
          }
      }
  }

  def main(args: Array[String]): Unit =
//    println("a")

//    println("main = ", listMonad.filterM(List(1, 2, 3, 4))(x => x))
    //    println(listMonad.replicateM(3, List(2)))
    //    println(listMonad._replicateM(3, List(2)))

    //    println(listMonad.filterM(List(1, 2, 3, 4))(x => List(x % 2 == 0)))
    //    println(
    //      optionMonad.filterM(List(Some(1), Some(2)))(x => Some(x.get % 2 == 0))
    //    )
    //    println(listMonad)
//    val s = stateMonad[Int]
//    val ss = stateMonad[Int]
//    s.replicateM(3, State[IntState])

//    def stateApply = (i: Int) => (i, i * 2)
//    // exercise 11.18
//    // replicateM: 同じステートの結果を State[S, List[A]] の形にまとめる
//    // map2: 複数のmapの結果を結合して新たなステートとしてる
//    // sequence: ステートのリストから、結果をリストにまとめたステートに変換する
//    val state = State.apply(stateApply)
//    val stateMonad = Chapter11.stateMonad[Int]
//    val value1 = stateMonad.unit(10)
//    val flm =
//      stateMonad.flatMap(value1)(a => State.apply(aa => (a * aa, aa * 2)))
//    val replicateM = stateMonad.replicateM(10, state)
//    val map2 = stateMonad.map2(state, state)(_ + _)
//    val sequence = stateMonad.sequence(List(state, state))
//    println(replicateM.run(5))
//    println(map2.run(5))
//    println(sequence.run(5))

    //    println(zipWithIndex(List(10, 20, 30)))
//    println(_zipWithIndex(List(10, 20, 30)))
    println(zipWithIndex2(List(10, 20, 30)))

//    // exercise 11.20
//    val f1 = (i: Int) => s"${i * 10} * 10"
//    val f2 = (i: Int) => s"${i * 20} * 20"
//    val readerMonad: Monad[
//      ({
//        type f[x] = Reader[Int, x]
//      })#f
//    ] = Reader.readerMonad
//    // flatMapは関数を実行した結果から再度 Readerを定義する
//    val fMapReader = Reader(f1)
//    val flatMapResult =
//      readerMonad.flatMap(fMapReader)(s => Reader(r => s"$s * ${r.toString}"))
//    println(s"result flatMap: ${flatMapResult.run(2)}")
//    val sequenceReader = List(Reader(f1), Reader(f2))
//    val sequenceResult = readerMonad.sequence(sequenceReader)
//    println(s"result sequence: ${sequenceResult.run(2)}")
//    val f3 = (i: Int) => Reader[Int, String](ii => s"${i + ii} *")
//    val joinReader: Reader[Int, Reader[Int, String]] = Reader(f3)
//    val joinResult = readerMonad.join(joinReader)
//    println(s"result join: ${joinResult.run(2)}")
//    val replicateMResult = readerMonad.replicateM(5, Reader(f1))
//    println(s"result replicateM: ${replicateMResult.run(2)}")
}
