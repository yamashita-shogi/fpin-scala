package Chapter10

import Chapter3.Chapter3.Tree

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

  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }

//  def optionMonoid[A] = new Monoid[Option[A]] {
//    def op(a1: Option[A], a2: Option[A]) = a1 + a2
//    val zero = null
//  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]) = x orElse y
    val zero = None
  }

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  // Now we can have both monoids on hand:
  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]
  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A) = a1.andThen(a2)
    val zero = A => A
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

//  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
//    as.fold(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldMapViafoldRight[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldRight(m.zero)((a, b) => m.op(f(a), b))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    v.length match {
      case 0 => m.zero
      case 1 => f(v(0))
      case _ =>
        val (l, r) = v.splitAt(v.length / 2)
        m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val mon = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]) =
        (o1, o2) match {
          case (Some((x1, y1, p)), Some((x2, y2, q))) =>
            Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
          case (x, None) => x
          case (None, x) => x
        }
      val zero = None
    }
    foldMapV(ints, mon)(i => Some((i, i, true))).map(_._3).getOrElse(true)
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    // The empty result, where we haven't seen any characters yet.
    val zero = Stub("")

    def op(a: WC, b: WC) = (a, b) match {
      case (Stub(c), Stub(d))       => Stub(c + d)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }
  }

  def count(s: String): Int = {
    def wc(c: Char): WC =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)

    def unstub(s: String) = s.length min 1

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s)       => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
      foldMap(as)(f.curried)(endoMonoid[B])(z)

    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
      foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
      foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)

    def toList[A](fa: F[A]): List[A] =
      foldRight(fa)(List[A]())(_ :: _)
  }

  object ListFoldable extends Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
      as.foldRight(z)(f)
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
      as.foldLeft(z)(f)
    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
  }

  object IndexedSeqFoldable extends Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
      as.foldRight(z)(f)
    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
      as.foldLeft(z)(f)
    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      foldMapV(as, mb)(f)
  }

  object StreamFoldable extends Foldable[Stream] {
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
      as.foldRight(z)(f)
    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
      as.foldLeft(z)(f)
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object TreeFoldable extends Foldable[Tree] {
    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
      as match {
        case Leaf(a)      => f(a)
        case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
      }
    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
      case Leaf(a)      => f(z, a)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match {
      case Leaf(a)      => f(a, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }
  }

  object OptionFoldable extends Foldable[Option] {
    override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
      as match {
        case None    => mb.zero
        case Some(a) => f(a)
      }
    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
      as match {
        case None    => z
        case Some(a) => f(z, a)
      }
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
      as match {
        case None    => z
        case Some(a) => f(a, z)
      }

  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]) = new Monoid[(A, B)] {
//    def op(a1: A, a2: A, b1: B, b2: B) = (A.op(a1, a2), B.op(b1, b2))
    def op(x: (A, B), y: (A, B)) =
      (A.op(x._1, y._1), B.op(x._2, y._2))
    val zero = (A.zero, B.zero)
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K, V]()
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
        }
    }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      def op(f: A => B, g: A => B) = a => B.op(f(a), g(a))
      val zero: A => B = a => B.zero
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val b: Monoid[Map[A, Int]] = mapMergeMonoid(intAddition)
    foldMapV(as, b)(i => Map(i -> 1))
  }

  def dbag[A](as: IndexedSeq[A]): Unit = {
    val b: Monoid[Map[A, Int]] = mapMergeMonoid(intAddition)
    println(foldMapV(as, b)(i => { println(i); Map(i -> 1) }))
  }

  def main(args: Array[String]): Unit = {
////    println("a")
//    val nums1 = IndexedSeq(5, 4, 3, 2, 5)
//    val nums2 = IndexedSeq(1, 2, 3, 4, 5)
//    println(ordered(nums1))
////    println(ordered(nums2))

//    val s = "lorem ipsum dolor sit amet, "
//    println(count(s))

//    val M: Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(
//      mapMergeMonoid(intAddition)
//    )

//    val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
//    val m2 = Map("o1" -> Map("i2" -> 3))
//    val m3 = M.op(m1, m2)
//    println(m3)

    println(bag(Vector("work", "rose", "is", "work", "rose")))

  }
}
