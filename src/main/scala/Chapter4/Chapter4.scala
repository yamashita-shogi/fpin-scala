package Chapter4

object Chapter4 {
  sealed trait Option[+A]
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

//  def mean(xs: Seq[Double]): Double =
//    if (xs.isEmpty)
//      throw new ArithmeticException("mean of empty list!")
//    else xs.sum / xs.length

  def mean_1(xs: IndexedSeq[Double], onEmpty: Double): Double =
    if (xs.isEmpty) onEmpty
    else xs.sum / xs.length

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def main(args: Array[String]): Unit = {
    println("d")
  }
}
