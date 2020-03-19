package Chapter2

object Chapter2 {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "the absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  // int -> int な関数
  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d id %d."
    msg.format(name, n, f(n))
  }

  // exercise 2.1
  // n番目のフィボナッチ数を取得する再帰関数を記述せよ。
  // 最初の2つのフィボナッチ数は0と1である。n番目の数字は常に前の2つの数字の合計となる。この数列は0、1、1、2、3、5のように始まる。
  // 再帰関数の定義では、ローカルな末尾再帰関数を使用すること。
  // 捜索対象の番の数。n番目
  def fib(n: Int): Int = {

    // ここにn番目のフィボナッチ数を探す処理
    // 最初の2つのフィボナッチ数の0と1を引数でもらう
    // カウンタは最初の0と1を返す以降の数字から。。
    @annotation.tailrec
    def go(i: Int, a: Int, b: Int): Int = {
      if (n <= 1) n
      else if (i < n) go(i + 1, b, a + b)
      else a + b
    }
    go(2, 0, 1)
  }

  // 配列内でStringを検索する単相関数
//  def findFirst(ss: Array[String], key: String): Int = {
//    println(ss(0))
//    @annotation.tailrec
//    def loop(n: Int): Int =
//      if (n >= ss.length) -1
//      else if (ss(n) == key) n
//      else loop(n + 1)
//
//    loop(0)
//  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  // exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    //@annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length - 1) true //最後まで回したらtrue
      else if (!ordered(as(n), as(n + 1)))
        false //希望の順番じゃなければfalse、!なしの場合はチェックしたい順番と逆の不等号関数にする
      else loop(n + 1)

    loop(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  // exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => ((b: B) => f(a, b))

  // exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a: A)(b: B)

  // exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def main(args: Array[String]): Unit = {
    // exercise 2.1
//    (1 to 10).foreach { i =,
//      println("fib %d : %d".format(i, fib(i)))
//    }
//    println(formatAbs(-42))
//    println(formatFactorial(7))
    //println(formatResult("absolute value", -42, abs))
    //println(formatResult("factroial", 7, factorial))
    //println(findFirst(Array("aa", "bb", "cc"), (x: String) => x == "dd")

    // exercise 2.2
    println(isSorted(Array(1, 2, 3, 4), (x: Int, y: Int) => x < y))
    println(isSorted(Array(1, 3, 2, 4), (x: Int, y: Int) => x < y))
    println(isSorted(Array(4, 3, 2, 1), (x: Int, y: Int) => x < y))
  }
}
