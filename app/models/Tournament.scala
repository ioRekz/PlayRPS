package models

import akka.actor._
import akka.routing.RoundRobinRouter
import akka.util.Duration
import akka.util.duration._
import akka.util.Timeout
import akka.pattern.ask
import akka.dispatch.Await
import scala.collection._
import play.api.Play.current
import play.api.libs.concurrent._
import play.api._
import scala.collection._
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
case class Result(winner : (String,String), looser : (String,String))
case class Games(games : List[Result])
case class Rounds(rounds : List[Games])
case class TourneyInfo(players: List[String], results: Rounds)
case class Register(player: String)
case class Remove(player: String)
case class Players(players: List[String])
case object GiveResults extends TournamentMessage
case object Test extends TournamentMessage



class Tournament(players: List[String], createGame: (String, String) => ValidGame, listener: ActorRef) extends Actor  {
	require(players.length%2==0)
	
  implicit val timeout = Timeout(2 seconds) 
  
	def receive = {
  
		case Start =>
			println("tourneyStart")
			startRound(players)
      listener ! TourneyStarted(players)
      println("tournament official name : "+self.path)
      
			
		case Winners(winners) =>
			if(winners.length > 1) {
				startRound(players.intersect(winners))
        self ! GiveResults
				}
			else {
				listener ! TourneyWinner(winners.head)
				context.stop(self)
			}
		
		case RankingMatch(winners, loosers) => 
			val roundNbr : Int = (log(players.length/winners.length) / log(2)).toInt
			//startRoundWithName(winners++loosers, roundNbr.toString, true)
      startRoundWithName(winners, roundNbr.toString, false)
    
    case GiveResults =>
      val firstSender = sender
      var rounds : List[Games] = Nil
      context.children.foreach { round =>
        (round ? GiveResults).asPromise.map {
          case g:Games =>
            rounds = rounds :+ g
            if(context.children.size == rounds.length)
              firstSender ! Rounds(rounds)
        }
      }
	} 
	
	override def preStart() {
	}
	
	override def postStop() {
	}
	
	def startRoundWithName(playerslist : List[String], name : String, finals : Boolean = false) {
		val round = context.actorOf(Props(new Round(playerslist, finals, createGame, listener)),
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
	var results : mutable.ListBuffer[Result] = mutable.ListBuffer.empty
  
  implicit val timeout = Timeout(2 seconds) 
	
  def receive = {
		case Start =>
			println("Round Start "+players)
			for(v <- players.sliding(2,2)) {				
				println("starting game for "+v(0)+v(1))
				val game = context.actorOf(Props(createGame(v(0),v(1))), name=v(0)+"-"+v(1))
				game ! Start
			}
			
		case Result(winner,looser) =>
			curwinners = curwinners + winner._1
      results += Result(winner,looser)
			//listener ! RoundWinner(winner._1, self.path.name.toInt)
			println(winner._1+" win in "+self.path.name)
			if(curwinners.size == players.length/2){
				if(curwinners.size == 2 && !finals) {
				//finals
					val loosers = players.diff(curwinners.toList)
					context.parent ! RankingMatch(curwinners.toList, loosers)
				} else context.parent ! Winners(curwinners.toList)
				//context.stop(self)
			}
      
      case GiveResults => 
        sender ! Games(results.toList)
	}
}

