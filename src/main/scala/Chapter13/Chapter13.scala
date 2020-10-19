package Chapter13

import Chapter11.Chapter11.Monad

import scala.io.StdIn.readLine

object Chapter13 {

  object IO0 {

    trait IO {
      self =>
      def run: Unit

      def ++(io: IO): IO = new IO {
        def run = {
          self.run; io.run
        }
      }
    }

    object IO {
      def empty: IO = new IO {
        def run = ()
      }
    }

    def fahrenheitToCelsius(f: Double): Double =
      (f - 32) * 5.0 / 9.0

    // Ordinary code with side effects
    def converter: Unit = {
      println("Enter a temperature in degrees Fahrenheit: ")
      val d = readLine.toDouble
      println(fahrenheitToCelsius(d))
    }
  }

  object IO1 {
    // リスト13-4 iomonad/IO.scala
    sealed trait IO[A] {
      self =>
      def run: A

      def map[B](f: A => B): IO[B] =
        new IO[B] {
          def run = f(self.run)
        }

      def flatMap[B](f: A => IO[B]): IO[B] =
        new IO[B] {
          def run = f(self.run).run
        }
    }

    // リスト13-5 iomonad/IO.scala
    // sealed traitのIOをmonadで拡張
    object IO extends Monad[IO] {
      def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
      def flatMap[A, B](fa: IO[A])(f: A => IO[B]) = fa flatMap f
      def apply[A](a: => A): IO[A] = unit(a)
    }

    // リスト13-6
    def ReadLine: IO[String] = IO { readLine }
    def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

    // object IO0 の関数 fahrenheitToCelsius を import
    import IO0.fahrenheitToCelsius
    def converter: IO[Unit] =
      for {
        _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
        d <- ReadLine.map(_.toDouble)
        _ <- PrintLine(fahrenheitToCelsius(d).toString)
      } yield ()
  }

  def main(args: Array[String]): Unit = {
    println("a")
//    import IO0._
    import IO1._
    converter

  }
}
