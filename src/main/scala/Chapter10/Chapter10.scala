package Chapter10

object Chapter10 {

  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  // 以下ならグレーのコメント出ない
//  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
//    def op(a1: List[A], a2: List[A]) = a1 ++ a2
//    val zero = Nil
//  }
  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

//  def optionMonoid[A] = new Monoid[Option[A]] {
//    def op(a1: Option[A], a2: Option[A]) = a1 + a2
//    val zero = null
//  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]) = x orElse y
    val zero = None
  }

  def main(args: Array[String]): Unit = {
    println()
  }
}
