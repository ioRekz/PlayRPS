package models

import akka.actor._
import akka.util.duration._
import akka.util.Duration
import play.api.libs.concurrent._
import play.api.libs.json._
import play.api.libs.iteratee._
import akka.util.Timeout
import akka.pattern.ask
import play.api.Play.current
import scala.collection.parallel.ParSet
import akka.dispatch.{ Future, Await }
import play.api.mvc._



case class Join(username: String, tournament: String, slots: Int)
case class Connected(enumerator:Enumerator[JsValue], player: ActorRef)
case class ConnectedWithLobby(enumerator:Enumerator[JsValue], player: ActorRef, lobby: ActorRef)
case class CannotConnect(msg: String)
case class NewGame(opponent: String, game : ActorRef)
case class Quit(username: String)
case class NotifyJoin(username: String, listplayers: List[String])
case class WinLost(winner: String, move : String, looser: String, moveL : String, round: Int)
case class JoinTourney(tn : TourneyInfo)
case class SpecTourney(tn: TourneyInfo)
case class Tell(msg: JsObject)
case class Registered(players: List[String])
case class Reconnect(channel : PushEnumerator[JsValue])
case class StopIfYouCan()
case object CheckMoves
case object GetLobbyName
case class LobbyInfo(name: String, nbPlayers: Int, slots: Int)
case class Lobbies(lobbies : List[LobbyInfo])
case class DrawRes(winner : String, looser: String, move: String)
case class Accepted(player: ActorRef)
case class Register(player: String)
case class Remove(player: String)
case class RegisterRobotIn(name: String, tourney: String)
case class RegisterRobot(name:String)
case class TourneyInfo(players: List[String], results: Rounds)
case class PersonalResult(winner: String, move : String, looser: String, moveL : String, round: Int)





object Chifoumi {

  val nbPlayer = 32

	implicit val timeout = Timeout(1 second)
	lazy val default = {
    Akka.system.actorOf(Props[Chifoumi],name="chifoumi")
  }


	// def getGames : Promise[List[LobbyInfo]] = {
    // (default ? GetLobbyName).asPromise.map {
      // case Lobbies(lobbies) =>
        // lobbies
    // }
  // }


	val clients : scala.collection.mutable.MutableList[PushEnumerator[JsValue]] = scala.collection.mutable.MutableList.empty
	def createHelloEnum() : PushEnumerator[JsValue] = {
		val enume = Enumerator.imperative[JsValue]()
		clients += enume
		println(clients)
		enume
	}

	def notifyJoinQuit(lobby: LobbyInfo)  {
		val lobbyJson = Json.toJson(
				Map(
					"name" -> Json.toJson(lobby.name),
					"nbPlayers" -> Json.toJson(lobby.nbPlayers),
					"slots" -> Json.toJson(lobby.slots),
					"html" -> Json.toJson(views.html.lobbyitem(lobby).toString.trim)
				)
			)
		clients.foreach {
			_.push(lobbyJson)

		}
	}

	def writeEvent(client: PushEnumerator[String], lines: String*) {
		// lines.foreach { line =>
			// var data = line
			// if(lines.tail == line)
				// data = data + "\n"
			// println(data)
			// client.push(data)
		// }
		client.push(lines(0))
		client.push("andTest\n")

	}

	def getLobbies = {
		(default ? GetLobbyName).asPromise.map {
			case Lobbies(lobbies) =>
				println(lobbies)
				lobbies
		}
	}

	def join(username:String, tournament: String, slots: Int):Promise[(Iteratee[JsValue,_],Enumerator[JsValue])] = {
    (default ? Join(username, tournament, slots)).asPromise.map {

      case ConnectedWithLobby(enumerator, player, lobby) =>
        val iteratee = Iteratee.foreach[JsValue] { event =>
          player ! Play(username, (event \ "move").as[String].toLowerCase)

        }.mapDone { _ =>
          lobby ! Quit(username)
        }

        (iteratee,enumerator)

      case CannotConnect(error) =>

        // Connection error

        // A finished Iteratee sending EOF
        val iteratee = Done[JsValue,Unit]((),Input.EOF)

        // Send an error and close the socket
        val enumerator =  Enumerator[JsValue](JsObject(Seq("kind" -> JsString("global"), "message" -> JsString(error)))).andThen(Enumerator.enumInput(Input.EOF))
        println("CannotConnect : "+error)

        (iteratee,enumerator)

    }

  }

  def createRobot(robotName:String, tournament:String) {
    default ! RegisterRobotIn(robotName, tournament)
  }

}

//case class PlayerInfo(name: String, socket: PushEnumerator[JsValue], actor: ActorRef)

class Chifoumi extends Actor {


  var members = Map.empty[String, PushEnumerator[JsValue]]
  implicit val timeout = Timeout(2 seconds)

  override def preStart() = {

  }


  def receive = {

    case Join(username, tournament, slots) => {
      // Create an Enumerator to write to this socket
      val channel =  Enumerator.imperative[JsValue]()

      // if(context.children.toList.exists(_.path.name == username)) {
        // //sender ! CannotConnect("This username is already used")
      // } else {
      val initier = sender
      val lobby = getLobby(tournament, slots)
      for(i <- 1 to slots-1){
        val name = "Robot"+i
        lobby ! RegisterRobot(name)
      }
      (lobby ? Register(username)).asPromise.map {
        case cC: CannotConnect =>
          initier ! cC
        case Connected(channel, player) =>
          initier ! ConnectedWithLobby(channel,player,lobby)
      }


        // registerPlayer(username, tournament, channel) match {
          // case Left(error) => sender ! CannotConnect(error)
          // case Right(playor) => sender ! Connected(channel, playor)
        // }

    }

    case GetLobbyName =>
     // var lobbies : Set[String] = Set.empty
      // var lobbies = context.children.toList.map { _.path.name }
			// aggregates { lobbies =>
				// println(lobbies)
				// sender ! Lobbies(lobbies)
			// }
			if(context.children.toList.isEmpty)
				sender ! Lobbies(Nil)
			else {
				val firstSender = sender
				var lobbies : ParSet[LobbyInfo] = ParSet.empty
				context.children.foreach { lobby =>
					(lobby ? GetLobbyName).asPromise.map {
						case lob:LobbyInfo =>
							lobbies = lobbies + lob
							println(lobbies)
							if(context.children.size == lobbies.size)
								firstSender ! Lobbies(lobbies.toList)
					}
				}
			}


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

    case RegisterRobotIn(robotName: String, tournament: String) =>
      getLobby(tournament) ! RegisterRobot(robotName)


	}



  def getLobby(name: String, slots: Int = 0) : ActorRef = {
    try {
      context.actorOf(Props(new Lobby(name, slots, self)),name=name)

    } catch {
      case _ : InvalidActorNameException =>
        context.actorFor(name)
    }
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

  def registerRobots(number : Int, tournament: String, lobby: ActorRef) = {
    if(!context.children.toList.exists(_.path.name == "Robot1-"+tournament))
    for(i <- 30 to number){
      val name = "Robot"+i
      //val playor = context.actorOf(Props(new RandomRobot(name)), name = name+"-"+tournament)
      //context.watch(playor)
      //subscribeTourney(name, tournament, playor)
      lobby
    }
  }
}

class Lobby(tournament: String, slots: Int, listener : ActorRef, var players : Set[String] = Set.empty) extends Actor {

  var myTourney : Option[ActorRef] = None
  var tourneyList : List[String] = Nil
  implicit val timeout = Timeout(2 seconds)
  import scala.util.Random

	override def preStart() = {
    Chifoumi.notifyJoinQuit(getInfo)
  }

	override def postStop() = {

	}

  def registerPlayer(name: String) : Either[String,(ActorRef, PushEnumerator[JsValue])] =  {
    val channel =  Enumerator.imperative[JsValue]()
/*    if(players.contains(name)) {
      myTourney match {
        case Some(tourney) =>
          val player = context.actorFor(name)
          player ! Reconnect(channel)
          Right((player, channel))

        case None =>
          Left("Username already used") //tournament not started : player exists
      }
    } else {
        val player = try {
            context.watch ( context.actorOf(Props(new Player(name,channel)), name =  name) )

          }
          catch {
            case _ : InvalidActorNameException =>
              println("Invalide state, user name alrdy used")
              context.actorFor(name)
          }
          player ! SpecTourney(TourneyInfo(tourneyList,r))
          Right((player, channel))
      }*/

      (players.contains(name), myTourney) match {
        case (true, Some(tourney)) =>
          val player = context.actorFor(name)
          player ! Reconnect(channel)
          Right((player, channel))

        case (true, None) => Left("Username already used") //tournament not started : player exists

        case (false, None) =>
          val player = try {
              context.watch ( context.actorOf(Props(new Player(name,channel)), name =  name) )

            }
            catch {
              case _ : InvalidActorNameException =>
                println("Invalide state, user name alrdy used")
                context.actorFor(name)
            }
            Right((player, channel))
        case (false, Some(tourney)) =>
          val player = try {
              context.watch ( context.actorOf(Props(new Player(name,channel)), name =  name) )

            }
            catch {
              case _ : InvalidActorNameException =>
                println("Invalide state, user name alrdy used")
                context.actorFor(name)
            }
          //spectate
          (tourney ? GiveResults).asPromise.map {
            case r:Rounds =>
              player ! SpecTourney(TourneyInfo(tourneyList,r))
          }
          Right((player, channel))
      }

  }



  def receive = {
    case Register(name: String) =>
      println("lobby path "+self.path)
      registerPlayer(name) match {
        case Left(error) =>
          sender ! CannotConnect(error)
        case Right(pInfo) =>
          val playor = pInfo._1
          sender ! Connected(pInfo._2,pInfo._1)

          checkIn(name, playor)
      }



    case RegisterRobot(name) =>
      try {
        val robot = context.actorOf(Props(new RandomRobot(name)), name = name)
        checkIn(name, robot)

      } catch {
        case _ => "No need to create robot"
      }

    case Terminated(tn) =>
      myTourney match {
        case Some(tourney) =>
          if(tourney == tn) {
            println("tourney is over")
            context.stop(self)
          } else println("dead "+tn.path.name)
        case None =>
          println("dead "+tn.path.name)
          players = players - tn.path.name
					if(players.size == 0)
						context.stop(self)
					Chifoumi.notifyJoinQuit(getInfo)
      }

    case WinLost(winner, winmove, looser, loosemove, round) =>
			notifyAll("result",
        "draw" -> Json.toJson(winmove == loosemove),
        "winner" -> Json.toJson(Map("name" -> winner, "move" -> winmove)),
        "looser" -> Json.toJson(Map("name" -> looser, "move" -> loosemove)),
        "round" -> Json.toJson(round)
        )

    case PersonalResult(winner, winmove, looser, loosemove, round) =>
      notifyThem(winner :: looser :: Nil, "persoresult",
        "draw" -> Json.toJson(winmove == loosemove),
        "winner" -> Json.toJson(Map("name" -> winner, "move" -> winmove)),
        "looser" -> Json.toJson(Map("name" -> looser, "move" -> loosemove)),
        "round" -> Json.toJson(round)
        )

    case NotifyJoin(joiner, allplayers) => {
      notifyThem( allplayers.filterNot(_ == joiner), "join", "user" -> Json.toJson(joiner))
    }

    case TourneyStarted(players) =>
      notifyAll("tourneyStart", "members" -> Json.toJson(players))

    case TourneyWinner(player) =>
      notifyThem(player :: Nil, "youwin", "player" -> Json.toJson(player))

    case Quit(username) =>
      context.actorFor(username) ! StopIfYouCan
      println("quit : "+username)
			notifyAll("quit", "user" -> Json.toJson(username))

    case Remove(name: String) =>
      players = players - name

    case GetLobbyName =>
      println("reponse from lobby "+tournament)
      sender ! LobbyInfo(tournament, players.size, slots)
  }

  def checkIn(name: String, playor: ActorRef) {
    self ! NotifyJoin(name, players.toList)
    myTourney match {
      case Some(tourney) =>
        println("Recconnect ?")
        (tourney ? GiveResults).asPromise.map {
          case r:Rounds =>
            playor ! JoinTourney(TourneyInfo(tourneyList,r))
        }
      case None =>
        players = players + name
				Chifoumi.notifyJoinQuit(getInfo)
        if(players.size == slots) {
          tourneyList = new Random().shuffle(players.toList)
          myTourney = Some(context.actorOf(Props(new Tournament(tourneyList,
            { new ValidChoumi(_,_) with BestOf3

            },
            self)), name=tournament+"-tournament"))
          context.watch(myTourney.get)
          myTourney.get ! Start
        } else playor ! Registered(players.toList)
    }
  }

	def getInfo : LobbyInfo = {
		LobbyInfo(tournament, players.size, slots)
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
      context.actorFor(p) ! Tell(json)
    }
  }

  def notifyAll(kind: String, elems: (String,JsValue)*) {
    val json = fluentJson(kind,elems)
    context.actorSelection("*") ! Tell(json)
  }
}

