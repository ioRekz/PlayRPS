package models

import akka.actor._
import akka.routing.RoundRobinRouter
import akka.util.Duration
import akka.util.duration._
import scala.collection._
import play.api.Play.current
import play.api.libs.concurrent._
import play.api._

import scala.math._

sealed trait TournamentMessage
case object Start extends TournamentMessage
case class Play(fstPlayer: String, scdPlayer: String) extends TournamentMessage
case class TourneyWinner(winner:String) extends TournamentMessage
case class RoundWinner(winner : String, round : Int)
case class GameWinner(winner : String)
case class Winners(winners: List[String]) extends TournamentMessage
case class RankingMatch(winners : List[String], loosers : List[String]) extends TournamentMessage
case class TourneyStarted(players: List[String])

object Tournament {

	var players : SortedSet[String] = SortedSet.empty
	var isStarted : Boolean = false
  var maxPlayers : Int = 16
  
  def registerPlayer(player : String, listener: ActorRef)(errors : String => Either[String, ActorRef], success : List[String] => Either[String, ActorRef]) : Either[String, ActorRef] = {
    if(players.contains(player))
      errors("You already registered for this tournament")
    else {
      players = players + player
      val savedList = players.toList
      if(players.size == maxPlayers) {
        val tourney = Akka.system.actorOf(Props(new Tournament(savedList,listener) with ChifoumiTrait))
        println("tourneypath : "+tourney.path)
        tourney ! Start
        players = SortedSet.empty

      }
      success(savedList)
    }
  }
}

class Tournament(players: List[String], listener: ActorRef) extends Actor  {
	self: Game =>
	require(players.length%2==0)
	
	def receive = {
		case Start =>
			println("tourneyStart")
			startRound(players)
      listener ! TourneyStarted(players)
      
			
		case Winners(winners) =>
			if(winners.length > 1) {
				startRound(players.intersect(winners))
				}
			else {
				listener ! TourneyWinner(winners.head)
				context.stop(self)
			}
		
		case RankingMatch(winners, loosers) => {
			val roundNbr : Int = (log(players.length/winners.length) / log(2)).toInt
			//startRoundWithName(winners++loosers, roundNbr.toString, true)
      startRoundWithName(winners, roundNbr.toString, false)
		}
	}
	
	override def preStart() {
		Tournament.isStarted = true
	}
	
	override def postStop() {
		Tournament.isStarted = false
		Tournament.players = SortedSet.empty
	}
	
	def startRoundWithName(playerslist : List[String], name : String, finals : Boolean = false) {
		val round = context.actorOf(Props(new Round(playerslist, finals, getGame _, listener)),
					name = name)
		round ! Start
	}
	
	def startRound(playerslist : List[String]) {
		val roundNbr : Int = (log(players.length/playerslist.length) / log(2)).toInt
		startRoundWithName(playerslist, roundNbr.toString)
	}

//x = ln(16/nbJ) / ln 2

}

class Round(players: List[String], finals : Boolean = false ,createGame: (String, String) => Actor, listener: ActorRef) extends Actor {
	var curwinners : Set[String] = Set.empty
	
	def receive = {
		case Start =>
			println("Round Start "+players)
			for(v <- players.sliding(2,2)) {				
				println("starting game for "+v(0)+v(1))
				val game = context.actorOf(Props(createGame(v(0),v(1))))
				game ! Start
			}
			
		case GameWinner(winner) =>
			curwinners = curwinners + winner
			listener ! RoundWinner(winner, self.path.name.toInt)
			println(winner+" win in "+self.path.name)
			if(curwinners.size == players.length/2){
				if(curwinners.size == 2 && !finals) {
				//finals
					val loosers = players.diff(curwinners.toList)
					context.parent ! RankingMatch(curwinners.toList, loosers)
				} else context.parent ! Winners(curwinners.toList)
				//context.stop(self)
			}
	}
}