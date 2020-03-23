package Chapter3

object Chapter3 {

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

    // exercise 3.11
    def sumLeft(as: List[Int]): Int = {
      foldLeft(as, 0)(_ + _)
    }

    def productLeft(as: List[Double]): Double = {
      foldLeft(as, 1.0)(_ * _)
    }

    def lengthLeft[A](as: List[A]): Int = {
      foldLeft(as, 0)((x, _) => x + 1)
    }

    // exercise 3.12
    def reverse[A](l: List[A]): List[A] = {
//      l match {
//        case Cons(h, t) => Cons(t, h)
//        case _          => l
//      }
      foldLeft(l, Nil: List[A])((acc, h) => Cons(h, acc))
    }

    // exercise 3.13
    def foldRightViaFoldleft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(reverse(l), z)((b, a) => f(a, b))

    def foldRight_1[A, B](l: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

    def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
      foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

    // exercise 3.14
    def appendLeft1[A](l: List[A], ll: List[A]): List[A] =
      foldRight(l, ll)(Cons(_, _))

    def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
      foldRight(l, r)(Cons(_, _))

    // exercise 3.15
    def concat[A](l: List[List[A]]): List[A] =
      foldRight(l, Nil: List[A])(append)

    // exercise 3.16
    def add1(l: List[Int]): List[Int] =
      foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

    // exercise 3.17
    def doubleToString(l: List[Double]): List[String] =
      foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

    // exercise 3.18
    // リストの各要素を変更しかつリストの構造をそのまま保つ総称関数map
    def map[A, B](l: List[A])(f: A => B): List[B] =
      foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

    // exercise 3.19
    def filter[A](l: List[A])(f: A => Boolean): List[A] =
      foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

    // exercise 3.20
    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
      concat(map(l)(f))

    // exercise 3.21
    def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
      flatMap(l)(a => if (f(a)) List(a) else Nil)

    // exercise 3.22
    def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
      case (Nil, _)                     => Nil
      case (_, Nil)                     => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
    }

    // exercise 3.23
    def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
      (a, b) match {
        case (Nil, _)                     => Nil
        case (_, Nil)                     => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
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
    println(List.sumLeft(List(1, 2, 3)))
    //println(List.productLeft(List(5.0, 5.0, 5.0)))
    //println(List.lengthLeft(List(1, 2, 3, 4)))

    // exercise 3.12
//    println(List.reverse(List(1, 2, 3)))

    //println(List.appendLeft(List(1,2,3),List(4,5,6)))
    //println(List.appendViaFoldRight(List(1,2,3),List(4,5,6)))

//    println(List.add1(List(1,2,3)))
  }
}
