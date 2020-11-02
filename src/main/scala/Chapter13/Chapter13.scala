package Chapter13

import Chapter11.Chapter11.Monad
import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par

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

  object IO2a {
    sealed trait IO[A] {
      def flatMap[B](f: A => IO[B]): IO[B] =
        FlatMap(this, f)
      def map[B](f: A => B): IO[B] =
        flatMap(f andThen (Return(_)))
    }
    case class Return[A](a: A) extends IO[A]
    case class Suspend[A](resume: () => A) extends IO[A]
    case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

    def printLine(s: String): IO[Unit] =
      Suspend(() => println(s))

////    forever
////    val p = IO.forever(printLine("Still going..."))
//    @annotation.tailrec
//    def run[A](io: IO[A]): A = io match {
//      case Return(a)  => a
//      case Suspend(r) => r()
//      case FlatMap(x, f) =>
//        x match {
//          case Return(a)     => run(f(a))
//          case Suspend(r)    => run(f(r()))
//          case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
//        }
//    }
  }

  object IO3 {
//    type TailRec[A] = Free[Function0,A]
//    type Async[A] = Free[Par,A]

    // Exercise 13.1
    sealed trait Free[F[_], A] {
      def flatMap[B](f: A => Free[F, B]): Free[F, B] =
        FlatMap(this, f)

      def map[B](f: A => B): Free[F, B] =
        flatMap(f andThen (Return(_)))
    }

    case class Return[F[_], A](a: A) extends Free[F, A]
    case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
    case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

    // Exercise 13.1
    def freeMonad[F[_]]: Monad[({ type f[a] = Free[F, a] })#f] =
      new Monad[({ type f[a] = Free[F, a] })#f] {
        def unit[A](a: => A) = Return(a)
        def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]) = fa flatMap f
      }

    // Exercise 13.2
    @annotation.tailrec
    def runTrampoline[A](a: Free[Function0, A]): A = (a) match {
      case Return(a)  => a
      case Suspend(r) => r()
      case FlatMap(x, f) =>
        x match {
          case Return(a)  => runTrampoline { f(a) }
          case Suspend(r) => runTrampoline { f(r()) }
          case FlatMap(a0, g) =>
            runTrampoline {
              a0 flatMap { a0 =>
                g(a0) flatMap f
              }
            }
        }
    }

    // Exercise 13.3
    def step[F[_], A](fr: Free[F, A]): Free[F, A] = fr match {
      case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
      case FlatMap(Return(x), f)     => step(f(x))
      case _                         => fr
    }

    def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = step(a) match {
      case Return(a)              => F.unit(a)
      case Suspend(r)             => r
      case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
      case _                      => sys.error("Impossible, since `step` eliminates these cases")
    }

    // リスト13-15 Parを持ってきた
    // https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/parallelism/Par.scala
    sealed trait Console[A] {
      // このConsole[A]をPar[A]として解釈。
      def toPar: Par[A]

      // このConsole[A]をFunction0[A]として解釈。
      def toThunk: () => A

    }
    case object ReadLine extends Console[Option[String]] {
      def toPar = Par.lazyUnit(run)
      def toThunk = () => run

      // ReadLineの両⽅のインタープリタによって使⽤されるヘルパー関数。
      def run: Option[String] =
        try Some(readLine())
        catch { case e: Exception => None }
    }

    case class PrintLine(line: String) extends Console[Unit] {
      def toPar = Par.lazyUnit(println(line))
      def toThunk = () => println(line)
    }

    object Console {
      type ConsoleIO[A] = Free[Console, A]

      def readLn: ConsoleIO[Option[String]] =
        Suspend(ReadLine)

      def printLn(line: String): ConsoleIO[Unit] =
        Suspend(PrintLine(line))
    }

    // リスト13-17
    // 任意の'F[A]'と'G[A]'間の変換。
    trait Translate[F[_], G[_]] { def apply[A](f: F[A]): G[A] }

    // Translate[F,G]に対して中置構⽂F ~> Gを使⽤できるようになる。
    type ~>[F[_], G[_]] = Translate[F, G]

    val consoleToFunction0 =
      new (Console ~> Function0) { def apply[A](a: Console[A]) = a.toThunk }
    val consoleToPar =
      new (Console ~> Par) { def apply[A](a: Console[A]) = a.toPar }

    def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit G: Monad[G]): G[A] =
      step(free) match {
        case Return(a)              => G.unit(a)
        case Suspend(r)             => t(r)
        case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
        case _                      => sys.error("Impossible; `step` eliminates these cases")
      }

    // runConsoleFunction0 と runConsolePar のための implicit val
    implicit val function0Monad = new Monad[Function0] {
      def unit[A](a: => A) = () => a
      def flatMap[A, B](a: Function0[A])(f: A => Function0[B]) = () => f(a())()
    }

    implicit val parMonad = new Monad[Par] {
      def unit[A](a: => A) = Par.unit(a)
      def flatMap[A, B](a: Par[A])(f: A => Par[B]) = Par.fork { Par.flatMap(a)(f) }
    }

    // リスト13-19
    // No Implicit found for parameter G: Monad[Function0]
    def runConsoleFunction0[A](a: Free[Console, A]): () => A =
      runFree[Console, Function0, A](a)(consoleToFunction0)

    def runConsolePar[A](a: Free[Console, A]): Par[A] =
      runFree[Console, Par, A](a)(consoleToPar)

  }

  def main(args: Array[String]): Unit = {
    println("a")
//    import IO0._
    import IO1._

    // 実行方法
    converter.run

  }
}
