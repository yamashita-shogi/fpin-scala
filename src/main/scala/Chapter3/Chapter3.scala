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
//    val x = List(1, 2, 3, 4, 5) match {
//      case Cons(x, Cons(2, Cons(4, _)))          => x
//      case Nil                                   => 42
//      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
//      case Cons(h, t)                            => h + sum(t)
//      case _                                     => 101
//    }
    // -> 3（3番めのケースに入るはず
  }

  def main: Unit = {
    println("a")
  }
}
