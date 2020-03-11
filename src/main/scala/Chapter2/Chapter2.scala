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

  def main(args: Array[String]): Unit =
    //println(fib(0))
    (1 to 10).foreach { i =>
      println("fib %d : %d".format(i, fib(i)))
    }
}
