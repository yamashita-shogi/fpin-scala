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
    val a = testDt.withDayOfMonth(2).getDayOfWeek
    println(a)
    //    val a = dt.getDayOfWeek

    if (a == 7)
      println(dt.minusDays(0))
    else {
      val s = 7 - a
      println(s)
//      println(dt.plusDays(s))
      println(testDt.minusDays(s))
    }

  }
}
