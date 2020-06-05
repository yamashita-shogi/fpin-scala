package Chapter6

object Chapter6 {
  trait RNG {
    def nextInt
      : (Int, RNG) // ランダムな `Int`を生成する必要があります。 後で他の関数を `nextInt`に関して定義します。
  }

  object RNG {

    // NB - this was called SimpleRNG in the book text

    case class SimpleRNG(seed: Long) extends RNG {
      def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `＆`はビットごとのANDです。 現在のシードを使用して、新しいシードを生成します。
        val nextRNG = SimpleRNG(newSeed) // 次の状態は、新しいシードから作成された「RNG」インスタンスです。
        val n = (newSeed >>> 16).toInt // `>>>`は、ゼロフィルの右バイナリシフトです。 値「n」は、新しい擬似ランダム整数です。
        (n, nextRNG) // 戻り値は、疑似ランダム整数と次の「RNG」状態の両方を含むタプルです。
      }
    }
  }

  // exercise 6.1
  // 改良
//  def nonNegativeInt(rng: RNG): (Int, RNG) = {
//    val (i, r) = rng.nextInt
//    i match {
//      case a if a < 0 => (-(a + 1), r)
//      case _          => (i, r)
//    }
//  }

  // 模範
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    println("nonNegativeInt", i, r)
    (if (i < 0) -(i + 1) else i, r)
  }

//  def double(rng: RNG): (Double, RNG) = {
//    val (i, r) = rng.nextInt
//    (if (i.toDouble <= 0 && i.toDouble > 1) i.toDouble else 0, r)
//  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = nonNegativeInt(rng)
    val (d, rr) = double(r)
    ((i, d), rr)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r) = double(rng)
    val (i, rr) = nonNegativeInt(r)
    ((d, i), rr)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, r) = double(rng)
    val (dd, rr) = double(r)
    val (ddd, rrr) = double(rr)
    ((d, dd, ddd), rrr)
  }

//  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
//    (0 to count).map { _ =>
//      val (i, r) = nonNegativeInt(rng)
//
//    }.toList,r
//  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0)
      (List(), rng)
    else {
      val (x, r1) = rng.nextInt
      val (xs, r2) = ints(count - 1)(r1)
      (x :: xs, r2)
    }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if (count <= 0)
        (xs, r)
      else {
        val (x, r2) = r.nextInt
        go(count - 1, r2, x :: xs)
      }
    go(count, rng, List())
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def double2: Rand[Double] = {
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))
  }

  def main(args: Array[String]): Unit = {
    val rng = RNG.SimpleRNG(42)
    val rng2 = rng.nextInt._2
    val rng3 = rng2.nextInt._2

    // exercise 6.1
//    println(nonNegativeInt(rng3))
//    println(nonNegativeInt1(rng3))

    // exercise 6.2
//    println(double(rng))
//    println(double(rng2))
//    println(double(rng3))

    // exercise 6.3
//    println(intDouble(rng))
//    println(doubleInt(rng))
//    println(double3(rng))

    // exercise 6.4
//    println(ints(3)(rng))

//    println(nonNegativeEven)
    println(double2)

  }
}
