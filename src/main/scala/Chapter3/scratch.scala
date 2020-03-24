package Chapter3

//ここでテスト

object scratch {

  sealed trait List[+A] //1
  case object Nil extends List[Nothing] //2
  case class Cons[+A](head: A, tail: List[A]) extends List[A] //3

  object List { //4
    def sum(ints: List[Int]): Int = ints match { //5
      case Nil         => 0 //6
      case Cons(x, xs) => x + sum(xs) //7
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil          => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs)  => x * product(xs)
    }

    def apply[A](as: A*): List[A] = //8
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    // exercise 3.1
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t)                            => h + sum(t)
      case _                                     => 101
    }
    // -> 3（3番めのケースに入るはず

    // exercise 3.2
    def tail[A](l: List[A]): List[A] = l match {
      case Nil         => Nil
      case Cons(_, xs) => xs
    }

    // exercise 3.3
    def setHead[A](l: List[A], h: A): List[A] = l match {
      case Nil        => Nil
      case Cons(_, t) => Cons(h, t)
    }

    // exercise 3.4
    def drop[A](l: List[A], n: Int): List[A] =
      if (n <= 0) l
      else
        l match {
          case Nil        => Nil
          case Cons(_, t) => drop(t, n - 1)
        }

    // exercise 3.5
    // 述語とマッチする場合に限りListからその要素までの要素を削除するdropWhileの実装
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
      l match {
        case Nil        => Nil
        case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
      }

    def append[A](a1: List[A], a2: List[A]): List[A] = {
      println(a1)
      a1 match {
        case Nil        => a2
        case Cons(h, t) => Cons(h, append(t, a2))
      }
    }

    // exercise 3.6
    def init[A](a1: List[A]): List[A] = {
      println(a1)
      a1 match {
        case Nil        => a1
        case Cons(h, t) => if (t == Nil) t else Cons(h, init(t))
      }
    }
    //    def init[A](a1: List[A]): List[A] = {
    //      println(a1)
    //      a1 match {
    //        case Nil          => a1
    //        case Cons(h, Nil) => Nil
    //        case Cons(h, t)   => Cons(h, init(t))
    //      }
    //    }

    // 第1引数と題2引数の型は同じなはず
    // dropWhileの引数リストを2つのグループにまとめるとscalaが推論できるようになる
    // dropWhile2(xs)(f)
    // dropWhile2(xs)が関数を返し、それをfで呼び出す
    def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] =
      as match {
        case Cons(h, t) if (f(h)) => dropWhile2(t)(f)
        case _                    => as
      }

    // as = 計算対象リスト
    // z = 累積値
    // f = 足し算 or 掛け算する関数
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      println("as=", as, "z=", z)
      as match {
        case Nil         => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }
    }

    def sum2(ns: List[Int]) =
      foldRight(ns, 0)((x, y) => x + y)

    def product2(ns: List[Double]) =
      foldRight(ns, 1.0)(_ * _)

    // exercise 3.9
    // 模範見ました
    def length[A](as: List[A]): Int = {
      println(as)
      foldRight(as, 0)((_, x) => x + 1)
    }

    // exercise 3.10
    @annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
      println("as=", as, "z=", z)
      as match {
        case Nil        => z
        case Cons(h, t) => foldLeft(t, f(z, h))(f)
      }
    }

    // exercise 3.12
    def reverse[A](l: List[A]): List[A] = {
      //      l match {
      //        case Cons(h, t) => Cons(t, h)
      //        case _          => l
      //      }
      foldLeft(l, Nil: List[A])((acc, h) => Cons(h, acc))
    }

    // exercise 3.14
    def appendRight[A](l: List[A], ll: List[A]): List[A] = {
      foldRight(l, ll)((acc, h) => Cons(acc, h))
    }

    def appendLeft[A](l: List[A], ll: List[A]): List[A] = {
      foldLeft(reverse(l), ll)((acc, h) => Cons(h, acc))
    }

  }
  def main(args: Array[String]): Unit = {
    //val r = List.dropWhile(List(1, 1, 1, 2, 3, 4, 5), (x: Int) => x == 1)
    //val r = List.init(List(1, 2, 3, 4, 5))

    //    val rr = List.product2(List(1.0, 2.0, 3.0))
    //    println(rr)
    //    println("----")
    //    val rr = List.product2(List(0.0, 2.0,2.0))
    //    println(rr)

    //    val r = List.foldRight(List(1, 2, 3), 1)(_ * _)
    //    println(r)

    //    val r = List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    //    println(r)
    //val r = List.length(List(1,2,3))
    //println(r)

    // exercise 3.10
    //    val rr = List.foldLeft(List(1, 2, 3), 1)(_ * _)
    //    println(rr)

    // exercise 3.11
    //println(List.sumLeft(List(1, 2, 3)))
    //println(List.productLeft(List(5.0, 5.0, 5.0)))
    //println(List.lengthLeft(List(1, 2, 3, 4)))

//    println("main = ", List.appendRight(List(1, 2, 3), List(4, 5, 6)))
    println("main = ", List.appendLeft(List(1, 2, 3), List(4, 5, 6)))

  }
}
