package Chapter12

import Chapter11.Chapter11.Functor

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
      fas.foldRight(unit(List[A]()))((a, acc) => map2(a, acc)(_ :: _))

    def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
      sequence(List.fill(n)(ma))

    // def product[A, B](fa: F[A], fb: F[A]): F[(A, B)]
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      map2(fa, fb)((_, _))
  }

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

    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(
        f: (A, B, C) => D): F[D] =
      apply(apply(map(fa)(f.curried))(fb))(fc)

    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(
        f: (A, B, C, D) => E): F[E] =
      apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)
  }

//  // Monad[F]をApplicative[F]の部分型（subtype）にすることが可能です
//  trait Monad[F[_]] extends Applicative[F] {
//    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
//      join(map(fa)(f))
//
//    def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)
//
//    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
//      a => flatMap(f(a))(g)
//
//    def map[A, B](fa: F[A])(f: A => B): F[B] =
//      flatMap(fa)(a => unit(f(a)))
//
//    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
//      flatMap(fa)(a => map(fb)(b => f(a, b)))
//  }

  def main(args: Array[String]): Unit =
    println("a")
}
