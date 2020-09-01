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
  }

  def main(args: Array[String]): Unit =
    println("a")
}
