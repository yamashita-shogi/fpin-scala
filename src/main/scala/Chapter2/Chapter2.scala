package Chapter2

object Chapter2 {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x:Int) = {
    val msg = "the absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args:Array[String]): Unit =
    println(formatAbs(-42))
}
