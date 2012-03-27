package models


import akka.actor._
import akka.routing.RoundRobinRouter
import akka.util.Duration
import akka.util.duration._
import play.api.libs.concurrent._
import play.api._
import play.api.libs.json._
import play.api.libs.iteratee._
import akka.util.Timeout
import akka.pattern.ask
import play.api.libs.json.Json

import play.api.Play.current

case class Join(username: String)
case class Connected(enumerator:Enumerator[JsValue], player: ActorRef)
case class CannotConnect(msg: String)
case class NewGame(opponent: String, game : ActorRef)
case class Quit(username: String)
case class NotifyJoin(username: String, listplayers: List[String])
case class WinLost(winner: String, move : String, looser: String, moveL : String, round: Int)
case class JoinTourney(players: List[String])
case class Tell(msg: JsObject)
case object Stop


object Chifoumi {

	implicit val timeout = Timeout(1 second)
	lazy val default = {
    println("CREate actor chifoumi")
    Akka.system.actorOf(Props[Chifoumi],name="chifoumi")
    
   }
	val rules = List("paper","cisor", "rock")
	
	
	
	def join(username:String):Promise[(Iteratee[JsValue,_],Enumerator[JsValue])] = {
    (default ? Join(username)).asPromise.map {
      
      case Connected(enumerator, player) => 
      
        // Create an Iteratee to consume the feed
        val iteratee = Iteratee.foreach[JsValue] { event =>
          println("$$$$$$$$$$$$$$$$$$$$$ "+player.path.name)
          player ! Play(username, (event \ "move").as[String].toLowerCase)
          
        }.mapDone { _ =>
          default ! Quit(username)
        }

        (iteratee,enumerator)
        
      case CannotConnect(error) => 
      
        // Connection error

        // A finished Iteratee sending EOF
        val iteratee = Done[JsValue,Unit]((),Input.EOF)

        // Send an error and close the socket
        val enumerator =  Enumerator[JsValue](JsObject(Seq("kind" -> JsString("global"), "message" -> JsString(error)))).andThen(Enumerator.enumInput(Input.EOF))
        println("errror")
        
        (iteratee,enumerator)
         
    }

  }
}

//case class PlayerInfo(name: String, socket: PushEnumerator[JsValue], actor: ActorRef)

class Chifoumi extends Actor {
  
  var members = Map.empty[String, PushEnumerator[JsValue]]
    
  override def preStart() = {
    
  }
  
	
  def receive = {
    
    case Join(username) => {
      // Create an Enumerator to write to this socket
      val channel =  Enumerator.imperative[JsValue]()
      registerRobots(15)
      if(context.children.toList.exists(_.path.name == username)) {
        sender ! CannotConnect("This username is already used")
      } else {
        registerPlayer(username, channel) match {
          case Left(error) => sender ! CannotConnect(error)
          case Right(playor) => sender ! Connected(channel, playor)
        }
      }
    }
		
		case NotifyJoin(joiner, allplayers) => {
      notifyThem( allplayers.filterNot(_ == joiner), "join", "user" -> Json.toJson(joiner))
    }
    
    case TourneyStarted(players) =>
      notifyAll("tourneyStart", "members" -> Json.toJson(players)) //CHEDCKKKK
		
		case TourneyWinner(player) => 
			notifyThem(player :: Nil, "youwin", "user" -> Json.toJson(player))
      //kill robots
      for(p <- context.children if p.path.name.contains("Robot"))
        context.stop(p)
		
    case WinLost(winner, winmove, looser, loosemove, round) => 
			notifyAll("result", 
        "winner" -> Json.toJson(Map("name" -> winner, "move" -> winmove)),
        "looser" -> Json.toJson(Map("name" -> looser, "move" -> loosemove)),
        "round" -> Json.toJson(round)
        )
		
		case RoundWinner(player, round) => 
      notifyAll("winner", "user" -> Json.toJson(player), "round" -> Json.toJson(round))
      
		case Terminated(actor) =>
      println("dead :"+actor.path.name)
		
		case Quit(username) => 
      context.actorSelection(username) ! Stop
			println("quit : "+username)
			notifyAll("quit", "user" -> Json.toJson(username))
		
	}

  def fluentJson(kind: String, elems: Seq[(String, JsValue)] ) : JsObject = {
     Json.toJson(
      Map(
        "kind" -> kind
      )
    ).as[JsObject] ++ Json.toJson(elems.toMap).as[JsObject]
  }
  
  def notifyThem( them : List[String], kind: String, elems: (String, JsValue)*) {
    val json = fluentJson(kind, elems)
    
    them.foreach { p => 
      context.actorSelection(p) ! Tell(json)
    }
  }
  
  def notifyAll(kind: String, elems: (String,JsValue)*) {
    val json = fluentJson(kind,elems)
    context.actorSelection("*") ! Tell(json)
  }
  
  def registerRobots(number : Int) = {
    if(!context.children.toList.exists(_.path.name == "Robot1"))
    for(i <- 1 to number){
      val name = "Robot"+i
      val playor = context.actorOf(Props(new RandomRobot(name)), name = name)
      context.watch(playor)
      subscribeTourney(name, playor)
    }
  }
  
  def subscribeTourney(username : String, playor: ActorRef) : Either[String, ActorRef] =  {
    Tournament.registerPlayer(username, self)(
      error => { 
        println(error)
        Left(error)
      },
      players => {
        println("tourneyplayers : "+players)
        playor ! JoinTourney(players)
        self ! NotifyJoin(username, players)
        Right(playor)
      }
    )
  }
  
  /**
    return error if fails, actorRef of player if he joined
  **/
  def registerPlayer(username: String, channel: PushEnumerator[JsValue]) : Either[String, ActorRef] = {
    val playor = context.actorOf(Props(new Player(username,channel)), name = username)
    subscribeTourney(username, playor)   
  }
}



trait Game {
	def getGame(p : String, v : String) : Actor
}

class RandomRobot(name: String) extends Actor {
	def receive = {
		case NewGame(opponent, game) => 
			sender ! Play(name, new scala.util.Random().shuffle(ChifoumiGame.rules).head) //ChifoumiGame.rules.shuffle.head)
	}
}

trait ChifoumiTrait extends Game {
	def getGame(player1 : String,player2: String) = ChifoumiGame(player1, player2)
}

object ChifoumiGame {
	var games = Map.empty[String,ActorRef]
  val rules = List("paper","cisor","rock")
	def apply(player1: String, player2 : String) = new ChifoumiGame(player1,player2)
}

class ChifoumiGame(player1 : String, player2 : String) extends Actor {

	var played : Option[(String,String)] = None
	val rules = ChifoumiGame.rules
	//var games = Map.empty[String, ActorRef]
	
	override def preStart() = {
		println("match start :"+ this)
	}
	
	override def postStop() = {
		println("match over :" + this)
	}
	
	def receive = {
    
		case Play(player, move) =>
			played match {
				case Some(play) =>
          val (lastplayer, lastmove) = play
					if(player != lastplayer) {
						val stronger : String = rules((rules.indexOf(lastmove)+1)%3)
            move match {
              case `lastmove` => 
                //draw, replay !
                self ! Start
              case `stronger` =>
                context.parent ! GameWinner(player)
                Chifoumi.default ! WinLost(player, move ,lastplayer, lastmove, context.parent.path.name.toInt)
              case _ => 
                context.parent ! GameWinner(lastplayer)
                Chifoumi.default ! WinLost(lastplayer, lastmove, player, move, context.parent.path.name.toInt)
            }
					}
				case None => played = Some(player,move)	
			}
			println(player + " plays " + move)	
		
    case Start => 
      played = None
      Akka.system.actorSelection("/user/chifoumi/"+player1) ! NewGame(player2, self)
      Akka.system.actorSelection("/user/chifoumi/"+player2) ! NewGame(player1, self)
      
    
		
	}
  
  
	
	override def toString() = {
		player1 + " vs " + player2
	}
}

class Player(name: String, channel : PushEnumerator[JsValue]) extends Actor {

  var currentGame : Option[ActorRef] = None
  
  override def preStart() = {
  }
    
  def receive = {
    case JoinTourney(players) =>
      notifyMe("infos", "user" -> Json.toJson(name), "members" -> Json.toJson(players), "started" -> Json.toJson(currentGame.isDefined))
      
    case NewGame(opponent, game) => 
      notifyMe("newGame", "firstPlayer" -> Json.toJson(name), "secondPlayer" -> Json.toJson(opponent))
      currentGame = Some(game)

    case Play(username, move) =>
      currentGame match {
        case Some(game) => game ! Play(name,move)
        case None => notifyMe("global", "message" -> Json.toJson("You are not playin any game"))
      }
      
    case Tell(msg) =>
      channel.push(msg)
      
    case Stop =>
      context.stop(self)
  }
  
  def notifyMe(kind: String, elems: (String,JsValue)*) {
    channel.push(
      Json.toJson(
        Map(
          "kind" -> kind
        )
      ).as[JsObject] ++ Json.toJson(elems.toMap).as[JsObject]
    )
  }

}

