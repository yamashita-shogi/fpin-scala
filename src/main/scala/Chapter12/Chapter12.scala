package Chapter12

import Chapter10.Chapter10.{Foldable, Monoid, dual, endoMonoid}
import Chapter11.Chapter11.Id.idMonad
import Chapter11.Chapter11.{Functor, IntStateMonad, Monad, State, get, set, stateMonad}

object Chapter12 {

  trait Applicative[F[_]] extends Functor[F] {
    // プリミティブコンビネータ
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

    def unit[A](a: => A): F[A]

    // 派⽣コンビネータ
    def map[A, B](fa: F[A])(f: A => B): F[B] =
      map2(fa, unit(()))((a, _) => f(a))

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

    def sequence[A](fas: List[F[A]]): F[List[A]] =
      fas.foldRight(unit(List[A]()))((a, acc) => map2(a, acc)((_: A) :: _))

    def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
      sequence(List.fill(n)(ma))

    // def product[A, B](fa: F[A], fb: F[A]): F[(A, B)]
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      map2(fa, fb)((_, _))

    ////     exercise 12.8用
    //    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    //      map2(fab, fa)(_(_))
    //    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    //      apply(map(fa)(f.curried))(fb)
    //    // exercise 12.8
    //    def product[G[_]](
    //        G: Applicative[G]): Applicative[({ type f[x] = (F[x], G[x]) })#f] = {
    //      val self = this
    //      new Applicative[({ type f[x] = (F[x], G[x]) })#f] {
    //        def unit[A](a: => A) = (self.unit(a), G.unit(a))
    //        override def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
    //          (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
    //      }
    //    }
    def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
      (ofa foldLeft unit(Map.empty[K, V])) {
        case (acc, (k, fv)) =>
          map2(acc, fv)((m, v) => m + (k -> v))
      }
  }

  // exercise 12.2
  trait _Applicative[F[_]] extends Functor[F] {
    // `map2` is implemented by first currying `f` so we get a function
    // of type `A => B => C`. This is a function that takes `A` and returns
    // another function of type `B => C`. So if we map `f.curried` over an
    // `F[A]`, we get `F[B => C]`. Passing that to `apply` along with the
    // `F[B]` will give us the desired `F[C]`.
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(map(fa)(f.curried))(fb)

    // We simply use `map2` to lift a function into `F` so we can apply it
    // to both `fab` and `fa`. The function being lifted here is `_(_)`,
    // which is the same as the lambda notation `(f, x) => f(x)`. That is,
    // It's a function that takes two arguments:
    //   1. A function `f`
    //   2. An argument `x` to that function
    // and it simply applies `f` to `x`.
    //x    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    //      map2(fab, fa)(_(_))
    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = {
      map2(fab, fa)((ab, f) => ab(f))
    }

    def unit[A](a: => A): F[A]

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      apply(unit(f))(fa)

    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
      apply(apply(map(fa)(f.curried))(fb))(fc)

    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
      apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)
  }

  val streamApplicative = new Applicative[Stream] {
    //  val streamApplicative = new _Applicative[Stream] {
    def unit[A](a: => A): Stream[A] =
      Stream.continually(a)

    override def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
      a zip b map f.tupled

    //    def sequence[A](lma: List[F[A]]): F[List[A]] =
    override def sequence[A](a: List[Stream[A]]): Stream[List[A]] =
      a.foldRight(unit(List[A]()))((a, acc) => map2(a, acc)(_ :: _))
  }

  def eitherMonad[E] = new Monad[({ type f[x] = Either[E, x] })#f] {
    def unit[A](a: => A) = Right(a)

    override def flatMap[A, B](e: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      e match {
        case Left(a)  => Left(a)
        case Right(a) => f(a)
      }
  }

  sealed trait Validation[+E, +A]

  case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

  case class Success[A](a: A) extends Validation[Nothing, A]

  def validationApplicative[E] =
    new Applicative[({ type f[x] = Validation[E, x] })#f] {
      def unit[A](a: => A) = Success(a)

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C) =
        (fa, fb) match {
          case (Success(a), Success(b)) => Success(f(a, b))
          case (Failure(h1, t1), Failure(h2, t2)) =>
            Failure(h1, t1 ++ Vector(h2) ++ t2)
          case (e @ Failure(_, _), _) => e
          case (_, e @ Failure(_, _)) => e
        }
    }

  // Monad[F]をApplicative[F]の部分型（subtype）にすることが可能です
  trait Monad[F[_]] extends Applicative[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
      join(map(fa)(f))

    def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)

    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
      a => flatMap(f(a))(g)

    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      flatMap(fa)(a => unit(f(a)))

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      flatMap(fa)(a => map(fb)(b => f(a, b)))
  }

  object Monad {
    def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))

      override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
        st flatMap f
    }
  }

  //  // exercise 12.13
  //  trait Traverse[F[_]] {
  //    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = ???
  //    // mapがないため
  ////      sequence(map(fa)(f))
  //
  //    def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
  //      traverse(fga)(ga => ga)
  //  }
  //
  case class Tree[+A](head: A, tail: List[Tree[A]])
//
  val listTraverse = new Traverse[List] {
    override def traverse[M[_], A, B](as: List[A])(f: A => M[B])(
        implicit M: Applicative[M]): M[List[B]] =
      as.foldRight(M.unit(List[B]()))((a, fbs) => M.map2(f(a), fbs)(_ :: _))
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](oa: Option[A])(f: A => G[B])(
        implicit G: Applicative[G]): G[Option[B]] =
      oa match {
        case Some(a) => G.map(f(a))(Some(_))
        case None    => G.unit(None)
      }
  }

  //
  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](ta: Tree[A])(f: A => G[B])(
        implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))
  }

  // exercise 12.14
  //  trait Traverse[F[_]] extends Functor[F] {
  trait Traverse[F[_]] extends Foldable[F] with Functor[F] { self =>
    def traverse[G[_], A, B](
        fa: F[A]
    )(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
      sequence(map(fa)(f))

    def sequence[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
      traverse(fga)(ga => ga)

    type Id[A] = A
    val idMonad = new Monad[Id] {
      def unit[A](a: => A) = a

      override def flatMap[A, B](a: A)(f: A => B): B = f(a)
    }

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      traverse[Id, A, B](fa)(f)(idMonad)

    def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
      traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(Monad.stateMonad)

    //    def zipWithIndex[A](ta: F[A]): F[(A, Int)] =
    //      traverseS(ta)((a: A) =>
    //        (for {
    //          i <- get[Int]
    //          _ <- set(i + 1)
    //        } yield (a, i))).run(0)._1
    //
    //    def _toList[A](fa: F[A]): List[A] =
    //      traverseS(fa)((a: A) =>
    //        (for {
    //          as <- get[List[A]]
    //          _ <- set(a :: as)
    //        } yield ())).run(Nil)._2.reverse

    def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
      traverseS(fa)((a: A) =>
        (for {
          s1 <- get[S]
          (b, s2) = f(a, s1)
          _ <- set(s2)
        } yield b)).run(s)

    override def toList[A](fa: F[A]): List[A] =
      mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

    def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
      mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

    def reverse[A](fa: F[A]): F[A] =
      mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

    override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
      mapAccum(as, z)((a, b) => ((a, b), f(b, a)))._2

    def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      (mapAccum(fa, toList(fb)) {
        case (a, Nil)     => sys.error("zip: Incompatible shapes.")
        case (a, b :: bs) => ((a, b), bs)
      })._1

    def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
      (mapAccum(fa, toList(fb)) {
        case (a, Nil)     => ((a, None), Nil)
        case (a, b :: bs) => ((a, Some(b)), bs)
      })._1

    def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
      (mapAccum(fb, toList(fa)) {
        case (b, Nil)     => ((None, b), Nil)
        case (b, a :: as) => ((Some(a), b), as)
      })._1

    // exercise 12.18
    // G product H
    // not enough arguments for method product: (fa: G[A], fb: G[B])G[(A, B)].
    // Unspecified value parameters: fb: G[NotInferredB]
    //    def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
    //                           (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    //      traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)

    // exercise 12.19
    def compose[G[_]](implicit G: Traverse[G]): Traverse[({ type f[x] = F[G[x]] })#f] =
      new Traverse[({ type f[x] = F[G[x]] })#f] {
        override def traverse[M[_]: Applicative, A, B](fa: F[G[A]])(f: A => M[B]) =
          self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
      }

    // exercise 12.20
    def composeM[F[_], G[_]](implicit F: Monad[F],
                             G: Monad[G],
                             T: Traverse[G]): Monad[({ type f[x] = F[G[x]] })#f] =
      new Monad[({ type f[x] = F[G[x]] })#f] {
        override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))

        override def flatMap[A, B](mna: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
          F.flatMap(mna)(na => F.map(T.traverse(na)(f))(G.join))
      }

//    def composeM[G[_], H[_]](implicit G: Monad[G],
//                             H: Monad[H],
//                             T: Traverse[H]): Monad[({ type f[x] = G[H[x]] })#f] =
//      new Monad[({ type f[x] = G[H[x]] })#f] {
//        def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))
//        override def flatMap[A, B](mna: G[H[A]])(f: A => G[H[B]]): G[H[B]] =
//          G.flatMap(mna)(na => G.map(T.traverse(na)(f))(H.join))
//      }
  }
  //  // exercise 12.14
//  trait _Traverse[F[_]] extends Foldable[F] with Functor[F] {
//    def traverse[G[_], A, B](
//        fa: F[A]
//    )(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
//      sequence(map(fa)(f))
//
//    def sequence[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
//      traverse(fga)(ga => ga)
//
//    // traverse をベースにして map を実装
//    // Identityの意味で、恒等性(f(x)=x)を表す
//    // 最も単純なApplicativeということで利用
//    type Id[A] = A
//    // Idモナドを作成(unit,flatMapを持つ Aのモナド化)
//    val idMonad = new Monad[Id] {
//      def unit[A](a: => A) = a
//
//      override def flatMap[A, B](a: A)(f: A => B): B = f(a)
//    }
//
//    //`traverse`を呼び出し、` Id`を `Applicative`として選択する
//    // Bがapplicativeのときにも対応できるよということ？
//    // implicitでApplicativeつけない場合と何が違うの？
//    // traverseを利用することでmapの一般化ができるということ
//    // (抽象化した共通項を見出し、他の方でも利用できるよう置換)
//    // traverse は map を作れるのでmapより汎用的
//    def map[A, B](fa: F[A])(f: A => B): F[B] =
//      traverse[Id, A, B](fa)(f)(idMonad)
//  }

  def main(args: Array[String]): Unit = {
    println("a")

//    // Streamのリストから、Streamをリストにまとめたものにする
//    println(
//      streamApplicative
//        .sequence(List(Stream(1, 2), Stream(3, 4)))
//        .foreach(println))
////    println(_streamApplicative.sequence(List(Stream(1, 2, 3, 4))))

  }
}
