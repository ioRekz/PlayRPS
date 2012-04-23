package models

object ValidChoumi {
  def apply(player1: String, player2: String) = new ValidChoumi(player1, player2)
  val rules = List("paper","cisor", "rock")
}

class ValidChoumi(player1: String, player2: String) extends ValidGame {

  var moves : Map[String,String] = Map.empty
	val rules = ValidChoumi.rules
  
  lazy val lobby = {
    context.actorFor("../../..")
  }
  
  override def receive = {
    // add your custom Events here 
    super.receive.orElse {
      case _ => println("unhandled")
    }
  }
  
  override def getPlayer1 = this.player1
  override def getPlayer2 = this.player2

  def playGame(player:String, move: String) : Option[Result] = {
    println(player + " plays " + move)
    moves = moves + (player -> move)
      if(moves.size == 2) {
        val (lastplayer, lastmove) = (moves - player).head
        val stronger : String = rules((rules.indexOf(lastmove)+1)%3)
        move match {
          case `lastmove` => 
            //draw, replay !
            startGame()
            lobby ! DrawRes(player, lastplayer, move)
            None
          case `stronger` =>
            val res = Result((player,move),(lastplayer,lastmove))
            lobby ! res
            Some(res)
            //setWinner(player,move,lastplayer,lastmove)
          case _ => 
            val res = Result((lastplayer,lastmove),(player, move))
            lobby ! res
            Some(res)
        }
        
      } else None
  }
  
  def startGame() {
      moves = Map.empty
      val tournamentN = "-" + context.actorFor("../..").path.name.split("-")(0)
      context.actorFor("/user/chifoumi/"+player1+tournamentN) ! NewGame(player2, self)
      context.actorFor("/user/chifoumi/"+player2+tournamentN) ! NewGame(player1, self)
  }
}
