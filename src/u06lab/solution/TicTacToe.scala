package u06lab.solution

object TicTacToe extends App {
  sealed trait Player{
    def other: Player = this match {case X => O; case _ => X}
    override def toString: String = this match {case X => "X"; case _ => "O"}
  }
  case object X extends Player
  case object O extends Player

  case class Mark(x: Int, y: Int, player: Player)
  type Board = List[Mark]
  type Game = List[Board]

  def find(board: Board, x: Int, y: Int): Option[Player] = board.foldLeft(Option.empty[Player])((a,b)=>if (isOccupied(b, x, y)) Option(b.player) else a)

  def isOccupied(a: Mark, x: Int, y: Int): Boolean = a.x == x && a.y == y

  def placeAnyMark(board: Board, player: Player): Seq[Board] = {
    var res: Seq[Board] = Seq.empty
    for (x <- 0 to 2; y <- 0 to 2) {
      if (find(board, x, y).isEmpty) {
        res = res :+ (board :+ Mark(x, y, player))
      }
    }
    res
  }

  def computeAnyGame(player: Player, moves: Int): Stream[Game] = moves match {
    case 0 => Stream(List(Nil))
    case _ => for {
      games <- computeAnyGame (player.other,moves -1)
      game  <- placeAnyMark(games.head, player)
    } yield if (isEnded(games.head)) games else game :: games
  }

  def isEnded(game: Board): Boolean = {
    if (game.size < 5) false
    else {
      var res = false
      for (y <- 0 to 2) {
        res = res || check(game)(m => m.x == y) || check(game)(m => m.y == y)
      }
      res || check(game)(m => m.y == m.x) || check(game)(m => (m.y +m.x) == 2)
    }
  }

  
  def check(b: Board)(filter: Mark => Boolean): Boolean = {
    val line = b.filter(filter).partition(m => m.player.toString == "X")
    if (line._1.size == 3 || line._2.size == 3 ) true else false
  }

  def printBoards(game: Seq[Board]): Unit =
    for (y <- 0 to 2; board <- game.reverse; x <- 0 to 2) {
      print(find(board, x, y) map (_.toString) getOrElse ("."))
      if (x == 2) { print(" "); if (board == game.head) println()}
    }

  // Exercise 1: implement find such that..
  println(find(List(Mark(0,0,X)),0,0)) // Some(X)
  println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),0,1)) // Some(O)
  println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),1,1)) // None

  // Exercise 2: implement placeAnyMark such that..
  printBoards(placeAnyMark(List(),X))
  //... ... ..X ... ... .X. ... ... X..
  //... ..X ... ... .X. ... ... X.. ...
  //..X ... ... .X. ... ... X.. ... ...
  printBoards(placeAnyMark(List(Mark(0,0,O)),X))
  //O.. O.. O.X O.. O.. OX. O.. O..
  //... ..X ... ... .X. ... ... X..
  //..X ... ... .X. ... ... X.. ...

  // Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  println("COMPUTE ANY GAME")
  var i: Int = 0
  computeAnyGame(O, 4) foreach {g => i = i+1;println("Game n.  " +i); printBoards(g); println()}
  //... X.. X.. X.. XO.
  //... ... O.. O.. O..
  //... ... ... X.. X..
  //              ... computes many such games (they should be 9*8*7*6 ~ 3000).. also, e.g.:
  //
  //... ... .O. XO. XOO
  //... ... ... ... ...
  //... .X. .X. .X. .X.

  // Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
  i = 0
  computeAnyGame(O, 6) foreach {g =>   i = i+1; if(g.size<=6) {println("Game n.  " +i + ", won in " +(g.size-1)+" moves"); printBoards(g); println()}}

}
