package Chapter3

object Chapter3 {

  sealed trait List[+A] //1
  case object Nil extends List[Nothing] //2
  case class Cons[+A](head: A, tail: List[A]) extends List[A] //3

  def main(args: Array[String]): Unit = {
    val r = List.init(List(1, 2, 3, 4, 5))
    println(r)
  }

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

    //    // exercise 3.5
    // 述語とマッチする場合に限りListからその要素までの要素を削除するdropWhileの実装
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
      l match {
        case Nil        => Nil
        case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
      }

    // exercise 3.5
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
  }
}
