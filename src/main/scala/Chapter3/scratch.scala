package Chapter3

object scratch {

  sealed trait List[+A] //1
  case object Nil extends List[Nothing] //2
  case class Cons[+A](head: A, tail: List[A]) extends List[A] //3

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

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

    // exercise 3.15
    def concat[A](l: List[List[A]]): List[A] =
      foldRight(l, Nil: List[A])(append)

    // exercise 3.16
    def plus_one(l: List[Int]): List[Int] =
      foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))
    //foldLeft(l, Nil: List[Int])((acc, h) => (acc, h + 1))

    // exercise 3.17
    def doubleToString(l: List[Double]): List[String] =
      foldRight(l, Nil: List[String])((d, t) => Cons(d.toString, t))

    // exercise 3.18
    def map[A, B](as: List[A])(f: A => B): List[B] =
      foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

    // exercise 3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

    // exercise 3.20
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      foldRight(as, Nil: List[B])((h, t) => append(f(h), t))

    // exercise 3.21
    def filter_flatMap[A](as: List[A])(f: A => Boolean): List[A] =
      flatMap(as)(h => if (f(h)) List(h) else Nil)

    // 3.22
    // 前までの流れからfold〜とか以前に作った関数使うかと思ってた…
    def sumListElem(l: List[Int], ll: List[Int]): List[Int] = {
      (l, ll) match {
        case (Nil, Nil)                   => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, sumListElem(t1, t2))
      }
    }

    def zipWith[A, B, C](l: List[A], ll: List[B])(f: (A, B) => C): List[C] = {
      (l, ll) match {
        case (Nil, _)                     => Nil
        case (_, Nil)                     => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
      }
    }

    def zipWith2[A](l: List[A], ll: List[A])(f: (A, A) => A): List[A] = {
      (l, ll) match {
        case (Nil, _)                     => Nil
        case (_, Nil)                     => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
      }
    }

    def filter_flatMap_revange[A, B](as: List[A])(f: A => Boolean): List[A] = {
      flatMap(as)(i => if (f(i)) List(i) else Nil)
    }

    @annotation.tailrec
    def startsWith[A](l: List[A], prefix: List[A]): Boolean = {
      println(l, prefix)
      (l, prefix) match {
        case (_, Nil)                              => true
        case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
        case _                                     => false
      }
    }

    @annotation.tailrec
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

      sup match {
        case Nil                       => sub == Nil
        case _ if startsWith(sup, sub) => true
        case Cons(h, t)                => hasSubsequence(t, sub)
      }
    }
  }

  object Tree {
    // exercise 3.25
//    def size[A](t: Tree[A]): Int = {
//      t match {
//        case Branch(left, _)  => size(left)
//        case Branch(_, right) => size(right)
//        case Leaf(_)          => 1
//      }
//    }
    def size[A](t: Tree[A]): Int = t match {
      case Leaf(_)      => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

    // exercise 3.26
    def maximum(t: Tree[Int]): Int = {
      println(t)
      t match {
        case Leaf(x)      => x
        case Branch(l, r) => maximum(l).max(maximum(r))
      }
    }

    // exercise 3.26
    def depth(t: Tree[Int]): Int = {
      println(t)
      t match {
        case Leaf(_)      => 0
        case Branch(l, r) => 1 + depth(l).max(depth(r))
      }
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
    //    println("main = ", List.appendLeft(List(1, 2, 3), List(4, 5, 6)))
    //println("main = ", List.plus_one(List(1, 2, 3)))
    //println("main = ", List.doubleToString(List(1.0, 2.0, 3.0)))
    //println("main = ", List.flatMap(List(1, 2, 3))(i => List(i, i)))

    //println("main = ", List.sumListElem(List(1, 2, 3), List(4, 5, 6)))
    //println("main = ", List.filter(List(1, 2, 3, 4))(x => x % 2 == 0))

//    // exersice 3.23
//    println(
//      "main = ",
//      List.zipWith(List(1, 2, 3), List(4, 5, 6))(_.toString + _.toString)
//    )
//
//    // exercise 3.24
//    println("main = ", List.hasSubsequence(List(1, 2, 3, 4), List(1, 2)))

//    // exercise 3.25
//    println(Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))))

//    // exercise 3.26
//    println(
//      Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))))
//    )

    // exercise 3.27
    println(
      Tree.depth(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))))
    )
  }
}
