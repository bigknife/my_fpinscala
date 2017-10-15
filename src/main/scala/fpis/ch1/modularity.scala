package fpis.ch1

object Modularity extends App {

  case class Player(name: String, score: Int)
  def printWinner(p: Player): Unit = println(s"${p.name} is the winner!")

  val sue = Player("Sue", 7)
  val bob = Player("Bob", 8)

  object NonFP {

    def declareWinner(p1: Player, p2: Player): Unit =
      if (p1.score > p2.score) printWinner(p1)
      else printWinner(p2)

    def test(): Unit =
      declareWinner(sue, bob)
  }

  object FP {
    def winner(p1: Player, p2: Player): Player =
      if (p1.score > p2.score) p1 else p2

    def declareWinner(p1: Player, p2: Player): Unit =
      printWinner(winner(p1, p2))

    def test(): Unit = {
      declareWinner(sue, bob)
      val players = List(
        Player("Sue", 7),
        Player("Bob", 8),
        Player("Joe", 9)
      )

      val p = players.reduceLeft(winner)
      printWinner(p)
    }

  }

  NonFP.test()
  FP.test()
}
