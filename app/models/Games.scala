package models

import akka.actor._
import play.api.Play.current
import play.api.libs.concurrent._
import akka.util.duration._

trait ValidGame extends Actor {

  def getPlayer1 : String = {""}
  def getPlayer2 : String = {""}
  
  

  //called when a player make a move and have to return a Result if possible
  def playGame(player: String, move: String) : Option[Result]
  
  //called when the game is ready to start
  def startGame()
  
  //def createGame(player1: String, player2: String) : ValidGame
  
  
  def setWinner(winner: String, winmove: String, looser: String, loosemov: String) {
    context.parent ! Result((winner,winmove),(looser,loosemov))
    //println("stoping game "+winner+" vs "+looser)
    //TODO let lobby be the gateway betw players/tourney println(context.actorFor("../../..").path.name)
    //context.stop(self)
  }
    
  def receive = {
    case Play(player, move) =>
      playGame(player, move).foreach { result =>

          println("result found "+result)
          setWinner(result.winner._1, result.winner._2, result.looser._1, result.looser._2)
      }
      
    case Start =>
      Akka.system.scheduler.scheduleOnce(2 seconds) {
        startGame()
      }
  }
}

trait Infos extends Actor {
  lazy val getRound : Int = {
    context.parent.path.name.toInt
  }
  
  lazy val lobby = {
    context.actorFor("../../..")
  }
}

object ValidChoumi {
  def apply(player1: String, player2: String) = new ValidChoumi(player1, player2)
  val rules = List("paper","cisor", "rock")
}

class ValidChoumi(player1: String, player2: String) extends ValidGame with Infos {

  var moves : Map[String,String] = Map.empty
	val rules = ValidChoumi.rules
  
  override def receive = {
    // add your custom Events here 
    super.receive.orElse {
      case _ => println("unhandled")
    }
  }
  
  override def getPlayer1 = this.player1
  override def getPlayer2 = this.player2
  
  override def setWinner(winner: String, winmove: String, looser: String, loosemov: String) {
    super.setWinner(winner,winmove,looser,loosemov)
    lobby ! WinLost(winner,winmove,looser,loosemov, getRound)
    lobby ! PersonalResult(winner,winmove,looser,loosemov, getRound)
  }

  def playGame(player:String, move: String) : Option[Result] = {
    println(player + " plays " + move)
    moves = moves + (player -> move)
      if(moves.size == 2) {
        val (lastplayer, lastmove) = (moves - player).head
        val stronger : String = rules((rules.indexOf(lastmove)+1)%3)
        move match {
          case `lastmove` => 
            //draw, replay !
            lobby ! PersonalResult(player, move,lastplayer, lastmove, context.parent.path.name.toInt)
            self ! Start
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
    context.actorFor("../../../"+player1) ! NewGame(player2, self)
    context.actorFor("../../../"+player2) ! NewGame(player1, self)
  }
}
