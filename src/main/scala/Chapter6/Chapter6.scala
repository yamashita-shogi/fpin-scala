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

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    if (rng.nextInt._1 < 0) (0, rng.nextInt._2)
  }

  def main(args: Array[String]): Unit = {
    val rng = RNG.SimpleRNG(42)
    println(rng.nextInt)
  }
}
