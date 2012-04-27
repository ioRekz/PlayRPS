package models

import akka.actor._
import akka.util.duration._
import play.api.libs.concurrent._
import play.api.libs.json._
import play.api.libs.iteratee._
import akka.util.Timeout
import akka.pattern.ask
import play.api.Play.current

case class Join(username: String, tournament: String)
case class Connected(enumerator:Enumerator[JsValue], player: ActorRef)
case class CannotConnect(msg: String)
case class NewGame(opponent: String, game : ActorRef)
case class Quit(username: String)
case class NotifyJoin(username: String, listplayers: List[String])
case class WinLost(winner: String, move : String, looser: String, moveL : String, round: Int)
case class JoinTourney(tn : TourneyInfo)
case class Tell(msg: JsObject)
case class Registered(players: List[String])
case class Reconnect(channel : PushEnumerator[JsValue])
case class StopIfYouCan()
case object CheckMoves
case object GetLobbyName
case class LobbyName(lobby: String)
case class Lobbies(lobbies : List[String])
case class DrawRes(winner : String, looser: String, move: String)

object Chifoumi {

	implicit val timeout = Timeout(1 second)
	lazy val default = {
    Akka.system.actorOf(Props[Chifoumi],name="chifoumi")
  }
	
	def getGames : Promise[List[String]] = {
    (default ? GetLobbyName).asPromise.map {
      case Lobbies(lobbies) =>
        lobbies
    }
  }
	
	def join(username:String, tournament: String):Promise[(Iteratee[JsValue,_],Enumerator[JsValue])] = {
    (default ? Join(username, tournament)).asPromise.map {
      
      case Connected(enumerator, player) => 
      
        val iteratee = Iteratee.foreach[JsValue] { event =>
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
  implicit val timeout = Timeout(2 seconds)
    
  override def preStart() = {
    
  }
  
	
  def receive = {
    
    case Join(username, tournament) => {
      // Create an Enumerator to write to this socket
      val channel =  Enumerator.imperative[JsValue]()
      registerRobots(15, tournament)
      // if(context.children.toList.exists(_.path.name == username)) {
        // //sender ! CannotConnect("This username is already used")
      // } else {
        registerPlayer(username, tournament, channel) match {
          case Left(error) => sender ! CannotConnect(error)
          case Right(playor) => sender ! Connected(channel, playor)
        }
      
    }
    
    case GetLobbyName => 
     // var lobbies : Set[String] = Set.empty
      var lobbies = context.children.toList.filter{_.path.name.endsWith("-lobby")}.map { lobby => 
        lobby.path.name.split("-lobby")(0)
      }
      sender ! Lobbies(lobbies)
      
		
		case NotifyJoin(joiner, allplayers) => {
      notifyThem( allplayers.filterNot(_ == joiner), "join", "user" -> Json.toJson(joiner))
    }
    
    case TourneyStarted(players) =>
      notifyAll("tourneyStart", "members" -> Json.toJson(players)) //CHEDCKKKK
		
		case TourneyWinner(player) => 
      //TODO pass tournament
			notifyThem(player+"-chifoumi" :: Nil, "youwin", "user" -> Json.toJson(player))
      //kill robots
      for(p <- context.children if p.path.name.contains("Robot"))
        context.stop(p)
		
    case WinLost(winner, winmove, looser, loosemove, round) => 
			notifyAll("result",
        "draw" -> Json.toJson(winmove == loosemove),
        "winner" -> Json.toJson(Map("name" -> winner, "move" -> winmove)),
        "looser" -> Json.toJson(Map("name" -> looser, "move" -> loosemove)),
        "round" -> Json.toJson(round)
        )
		
		case RoundWinner(player, round) => 
      //notifyAll("winner", "user" -> Json.toJson(player), "round" -> Json.toJson(round))
      
		case Terminated(actor) =>
      println("dead :"+actor.path.name)
		
		case Quit(username) => 
      context.actorSelection(username) ! StopIfYouCan
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
  
  def registerRobots(number : Int, tournament: String) = {
    if(!context.children.toList.exists(_.path.name == "Robot1-"+tournament))
    for(i <- 1 to number){
      val name = "Robot"+i
      val playor = context.actorOf(Props(new RandomRobot(name)), name = name+"-"+tournament)
      context.watch(playor)
      subscribeTourney(name, tournament, playor)
    }
  }
  
  def subscribeTourney(username : String, tournament : String, playor: ActorRef) : Either[String, ActorRef] =  {

    val lobby : Either[String,ActorRef] = try { 
      Right(context.actorOf(Props(new Lobby(tournament, 16, self)),name=tournament+"-lobby"))
    }
    catch {
      case e : InvalidActorNameException if e.getMessage.contains("unique") => 
        Right(context.actorFor(tournament+"-lobby"))
        
      case ee : InvalidActorNameException if !ee.getMessage.contains("unique") =>
        Left(ee.getMessage)
        
      case err => Left(err.getMessage)
    }
    
    
    lobby match {
      case Right(lob) =>
        (lob  ? Register(username)).asPromise.map {
          case Players(players) =>
            playor ! Registered(players)
            self ! NotifyJoin(username, players)
            
          case tn @ TourneyInfo(players, results) =>
            println("results from tourney "+results)
            playor ! JoinTourney(tn)
            self ! NotifyJoin(username, players)   
        }
        Right(playor)
      case Left(msg) =>
        Left(msg)
    }
  }
  
  /**
    return error if fails, actorRef of player if he joined
  **/
  def registerPlayer(username: String, tournament: String, channel: PushEnumerator[JsValue]) : Either[String, ActorRef] = {
    val actorName = username + "-" +tournament
    val playor = try { 
                  context.actorOf(Props(new Player(username,channel)), name = actorName) 
                }
                catch {
                  case _ : InvalidActorNameException => 
                    println("reconnect "+context.actorFor(actorName).path)
                    context.actorFor(actorName)

                    
                }
    playor ! Reconnect(channel)
    
    subscribeTourney(username, tournament, playor)   
  }
}

class Lobby(tournament: String, slots: Int, listener : ActorRef, var players : Set[String] = Set.empty) extends Actor {

  var myTourney : Option[ActorRef] = None
  var tourneyList : List[String] = Nil
  implicit val timeout = Timeout(2 seconds)
  import scala.util.Random
  
  def receive = {
    case Register(name: String) =>
      println("lobby path "+self.path)
      val initier = sender
      myTourney match {
        case Some(tourney) => 
          (tourney ? GiveResults).asPromise.map {
            case r:Rounds =>
              initier ! TourneyInfo(tourneyList,r)
          }
        case None =>
          players = players + name
          if(players.size == slots) {
            tourneyList = new Random().shuffle(players.toList)
            myTourney = Some(context.actorOf(Props(new Tournament(tourneyList, 
              { new ValidChoumi(_,_)
               
              }, 
              listener)), name=tournament+"-tournament"))
            context.watch(myTourney.get)
            myTourney.get ! Start
          }
          sender ! Players(players.toList)
      }

    case Terminated(tn) =>
      context.stop(self)
    
    case wn: WinLost => 
      Chifoumi.default ! wn
        
    
    case Remove(name: String) =>
      players = players - name
      
    case GetLobbyName =>
      println("reponse from lobby "+tournament)
      sender ! LobbyName(tournament)
  }
}
