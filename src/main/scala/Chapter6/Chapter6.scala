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

  // val int: Rand[Int] = rng => rng.nextInt　　の短縮形
  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  // 自分
  def double2: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  // 模範
  val _double: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  // 杉澤さん
  def doubleViaMap(rng: RNG): (Double, RNG) =
    map(nonNegativeInt)(_ / Int.MaxValue.toDouble + 1)(rng)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randIntDoubledd: Rand[(Int, Double)] = map2(int, double)((_, _))
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)
//
//  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
//    fs.foldRight(Nil)(i => i.)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def _ints(count: Int): Rand[List[Int]] =
//    sequence(List.fill(count)(nonNegativeInt))
    sequence(List.fill(count)(_.nextInt))

//  // ここから復習
//  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
//    val (i, rng2) = nonNegativeInt(rng)
//    val mod = i % n
//    if (i + (n - 1) - mod >= 0)
//      (mod, rng2)
//    else nonNegativeLessThan(n)(rng)
//  }

  // 薄っらみんなの見たから取っ掛かりがあった
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (r, rng2) = f(rng)
      g(r)(rng2)
    }

//  def _nonNegativeLessThan(n: Int): Rand[Int] =
//    flatMap(nonNegativeInt) { i =>
//      val mod = i % n
//      if (i + (n - 1) - mod >= 0)
//        (mod, rng2)
//      else _nonNegativeLessThan(n)(rng)
//    }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  // 杉澤さん
  def anonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }(rng)
  }

  // 自作
//  def mapViaflatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
//    flatMap(s) { rng =>
//      val (a, rng2) = s(rng)
//      (f(a), rng2)
//    }
//  }
//
//  def map2ViaflatMap[A, B, C](ra: Rand[A], rb: Rand[B])(
//      f: (A, B) => C): Rand[C] = {
//    flatMap((ra, rb)) { rng =>
//      {
//        val (a, rng2) = ra(rng)
//        val (b, rng3) = rb(rng2)
//        (f(a, b), rng3)
//      }
//    }
//  }

  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
//      flatMap(a => unit(f(a)))
      flatMap(a => State.unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State(s => {
        val (a, s1) = run(s)
        f(a).run(s1)
      })

  }

  object State {
//    type State[S, +A] = S => (A, S)
    type Rand[A] = State[RNG, A]

    def unit[S, A](a: A): State[S, A] =
      State(s => (a, s))

    def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
      sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

    def modify[S](f: S => S): State[S, Unit] =
      for {
        s <- get
        _ <- set(f(s))
      } yield ()

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  }

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  object Candy {
    type Rand[A] = State[RNG, A]
//    import State._

    def modify[S](f: S => S): State[S, Unit] =
      for {
        s <- get
        _ <- set(f(s))
      } yield ()

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    def update =
      (i: Input) =>
        (s: Machine) =>
          (i, s) match {
            case (_, Machine(_, 0, _))        => s
            case (Coin, Machine(false, _, _)) => s
            case (Turn, Machine(true, _, _))  => s
            case (Coin, Machine(true, candy, coin)) =>
              Machine(false, candy, coin + 1)
            case (Turn, Machine(false, candy, coin)) =>
              Machine(true, candy - 1, coin)
      }

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
      for {
        _ <- State.sequence(inputs map (State.modify[Machine] _ compose update))
        s <- State.get
      } yield (s.coins, s.candies)
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

//    println(nonNegativeEven(rng2))

    // exercise 6.5
//    println(double2(rng))
//    println(_double(rng))

    // exercise 6.6
//    println(randIntDouble(rng))
//    println(randIntDoubledd(rng))

    // exercise 6.7
//    println(_ints(3)(rng))

//    println(sequence(List(unit(1), unit(2), unit(3)))(r)._1)

    println(
      Candy
        .simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
    )
  }
}
