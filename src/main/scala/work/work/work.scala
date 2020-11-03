package work

import org.joda.time.DateTime

object work {
  private[this] def getLastWeekSunday(): Option[DateTime] = ???
  private[this] def getLastWeekMonday(): Option[DateTime] = ???
//    import org.joda.time.DateTime
//    val dt = new DateTime
//    if dt.getDayOfWeek() == 7

  def main(args: Array[String]): Unit = {
    import org.joda.time.DateTime
    val dt = new DateTime

    // 曜日（月曜=1, 火曜=2, 水曜=3 ・・・ 日曜=7）
    println(dt.getDayOfWeek)

    val testDt = new DateTime

    (1 to 30).foreach { i =>
      // 11/1
      val a = testDt.withDayOfMonth(i)

      // 11/ほげ の曜日
      val b = a.getDayOfWeek
      //    println(a)
      //    val a = dt.getDayOfWeek
      println(a, "曜日", b)

      if (b == 7) {
        println("sun")
        println(a)
      } else {
//        val s = 7 - b - 7 //plusDayでやりたいならマイナスを作るこれ
//        println(s)
//        println(a.plusDays(s))
        println(a.minusDays(b)) //minusDayでやりたいなら曜日の数字をそのまま引けばいいのでこれ
      }

      println("-----")
    }

  }
}
