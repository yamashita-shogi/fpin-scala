package Chapter1

object Chapter1 extends App {


  //  class Cafe {
  //    def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
  //      val cup = new Caffee()
  //      (cup, Charge(cc, cup.price))
  //    }
  //
  //    def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
  //      val purcharses: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
  //      val (coffees, charges) = purcharses.unzip(coffees, charges.reduce(c1, c2) => c1.combine(c2)
  //      )
  //    }
  //  }
  //
  //  case class Charge(cc: CreditCard, amount: Double) {
  //    def combine(other: Charge): Charge =
  //      if (cc == other.cc)
  //        Charge(cc, amount + other.amount)
  //      else
  //        throw new Exception("Can't combine charges to different cards")
  //  }
  //
  //  def coalesce(charges: List[Charge]): List[Charge] =
  //    charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
  //
  //  def f[A,B] (a: A): B = ???
  //


  println("chapter1")
}
