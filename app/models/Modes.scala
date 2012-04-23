package models

import akka.util.duration._
import play.api.libs.concurrent._
import play.api.Play.current

trait TwoInARow extends ValidGame {

  var lastwinner : Option[String] = None

  abstract override def startGame() = super.startGame()
  abstract override def playGame(player:String, move:String) : Option[Result] = {
    val resOp = super.playGame(player,move)
    resOp match { 
      case Some(res) => 
        lastwinner.foreach { lastwin =>
          if(lastwin == res.winner._1) {
            lastwinner = None
            return Some(res)
          }
        }
        lastwinner = Some(res.winner._1)
        startGame()
        None
      case None => None
    }
  }
}

trait Timed extends ValidGame {
  val timeToPlay = 10 seconds
  var played : Map[String, String] = Map.empty
  
  abstract override def startGame() = {
    Akka.system.scheduler.scheduleOnce(timeToPlay) {
      checkMoves()
    }
    super.startGame()
  }
  
  abstract override def playGame(player: String, move: String) = {
    played = played + (player -> move)
    super.playGame(player, move)
  }
  
  def checkMoves() {
    val player1 = getPlayer1
    val player2 = getPlayer2
    played.size match {
      case 1 =>
        if(played.contains(player1)) {
          setWinner(player1, played(player1), player2, "X")
        } else setWinner(player2, played(player2), player1, "X")
      
      case _ => //2 elimines
    }
  }
}

trait BestOf3 extends ValidGame {
  
  var wins : List[String] = Nil
  
  abstract override def startGame() = super.startGame()
  
  abstract override def playGame(player:String, move:String) = {
    val resOp = super.playGame(player,move)
    resOp match {
      case Some(res) => 
        if(wins.contains(res.winner._1)) {
          wins = Nil
          Some(res)
          
        }
        else {
          wins = wins :+ res.winner._1
          startGame()
          None
        }
        
      case None => None
    }
  }
}