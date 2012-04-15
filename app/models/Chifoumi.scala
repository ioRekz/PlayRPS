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

object Chifoumi {

	implicit val timeout = Timeout(1 second)
	lazy val default = {
    println("CREate actor chifoumi")
    Akka.system.actorOf(Props[Chifoumi],name="chifoumi")
    
   }
	val rules = List("paper","cisor", "rock")
	
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
    // Tournament.registerPlayer(username, tournament, self)(
      // error => { 
        // println(error)
        // Left(error)
      // },
      // players => {
        // playor ! JoinTourney(players)
        // self ! NotifyJoin(username, players)
        // Right(playor)
      // }
    // )
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
  var moves : Map[String,String] = Map.empty
	val rules = ChifoumiGame.rules
  val timeToPlay = 5 seconds
	//var games = Map.empty[String, ActorRef]
	
	override def preStart() = {
		println("match start :"+ this)
	}
	
	override def postStop() = {
		println("match over :" + this)
	}
	
	def receive = {
    
		// case Play(player, move) =>
			// played match {
				// case Some(play) =>
          // val (lastplayer, lastmove) = play
					// if(player != lastplayer) {
						// val stronger : String = rules((rules.indexOf(lastmove)+1)%3)
            // move match {
              // case `lastmove` => 
                // //draw, replay !
                // self ! Start
              // case `stronger` =>
                // result = Some(Result((player,move),(lastplayer, lastmove)))
                // context.parent ! Result((player, move),(lastplayer , lastmove))
                // Chifoumi.default ! WinLost(player, move ,lastplayer, lastmove, context.parent.path.name.toInt)
              // case _ => 
                // result = Some(Result((lastplayer,lastmove),(player, move)))
                // context.parent ! Result((lastplayer,lastmove),(player,move))
                // Chifoumi.default ! WinLost(lastplayer, lastmove, player, move, context.parent.path.name.toInt)
            // }
					// }
				// case None => played = Some(player,move)	
			// }
			// println(player + " plays " + move)	
		
    case Play(player, move) => 
      moves = moves + (player -> move)
      if(moves.size == 2) {
        val (lastplayer, lastmove) = (moves - player).head
        val stronger : String = rules((rules.indexOf(lastmove)+1)%3)
        move match {
          case `lastmove` => 
            //draw, replay !
            self ! Start
          case `stronger` =>
            setWinner(player,move,lastplayer,lastmove)
          case _ => 
            setWinner(lastplayer, lastmove, player, move)
        }
        
      }
      println(player + " plays " + move)	
    
    case Start => 
      played = None
      moves = Map.empty
      val tournamentN = "-" + context.actorFor("../..").path.name.split("-")(0)
      println("tournament namee "+tournamentN)
      context.actorFor("/user/chifoumi/"+player1+tournamentN) ! NewGame(player2, self)
      context.actorFor("/user/chifoumi/"+player2+tournamentN) ! NewGame(player1, self) 
      Akka.system.scheduler.scheduleOnce(timeToPlay) {
        self ! CheckMoves
      }
      // println(new Date())
    case CheckMoves =>
      moves.size match {
        case 1 =>
          if(moves.contains(player1)) {
            setWinner(player1, moves(player1), player2, "X")
          } else setWinner(player2, moves(player2), player1, "X")
        
        case 0 => //2 elimines
      }    
	}
  
  def setWinner(winner: String, winmove: String, looser: String, loosemov: String) {
    context.parent ! Result((winner,winmove),(looser,loosemov))
    Chifoumi.default ! WinLost(winner, winmove, looser, loosemov, context.parent.path.name.toInt)
    context.stop(self)
  }
	
	override def toString() = {
		player1 + " vs " + player2
	}
}

class Player(name: String, var channel : PushEnumerator[JsValue]) extends Actor {

  var currentGame : Option[ActorRef] = None
   
  
  override def preStart() = {
  }
    
  def receive = {
    case JoinTourney(infos) =>
      notifyMe("infos", "user" -> Json.toJson(name), "members" -> Json.toJson(infos.players), "started" -> Json.toJson(currentGame.isDefined),
        "results" -> parseRes(infos.results))
      
    case Registered(players) =>
      notifyMe("infos", "user" -> Json.toJson(name), "members" -> Json.toJson(players), "started" -> Json.toJson(currentGame.isDefined) )
     
    case NewGame(opponent, game) => 
      notifyMe("newGame", "firstPlayer" -> Json.toJson(name), "secondPlayer" -> Json.toJson(opponent))
      currentGame = Some(game)

    case play @ Play(username, move) =>
      currentGame match {
        case Some(game) => game ! play
        case None => notifyMe("global", "message" -> Json.toJson("You are not playin any game"))
      }
      
    case Reconnect(channel) => 
      this.channel = channel
      currentGame.foreach { game => 
        game.path.name.split("-").foreach { p =>
          if(p != name)
            self ! NewGame(p,game)
        }
      }
    // case Register(tournament: String) =>
      // Tournament.register(name, tournament, self)    
    
    case Tell(msg) =>
      channel.push(msg)
      
    case StopIfYouCan => 
      if(!currentGame.isDefined)
        context.stop(self)
      else println("CANT kill myself")
  }
  
  def parseRes(rounds: Rounds) : JsValue = {
    val resjson = rounds.rounds.map { games =>
      val allgames = games.games.map { result =>
        Json.toJson(
          Map(
            "winner" -> Map(
              "name" -> result.winner._1,
              "move" -> result.winner._2
            ),
            "looser" -> Map (
              "name" -> result.looser._1,
              "move" -> result.looser._2
            )
          )
        )
      }
      Json.toJson(Map("matches" -> allgames))
    }
    val results = Json.toJson(Map("rounds" -> resjson))
    results
  }
  
  def notifyMe(kind: String, elems: (String,JsValue)*) {
    println( Json.toJson(
        Map(
          "kind" -> kind
        )
      ).as[JsObject] ++ Json.toJson(elems.toMap).as[JsObject])
    channel.push(
      Json.toJson(
        Map(
          "kind" -> kind
        )
      ).as[JsObject] ++ Json.toJson(elems.toMap).as[JsObject]
    )
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
            myTourney = Some(context.actorOf(Props(new Tournament(tourneyList, {new ValidChoumi(_,_)} , listener)), name=tournament+"-tournament"))
            context.watch(myTourney.get)
            myTourney.get ! Start
          }
          sender ! Players(players.toList)
      }
    
    case Terminated(tn) =>
      context.stop(self)
        
    case Remove(name: String) =>
      players = players - name
      
    case GetLobbyName =>
      println("reponse from lobby "+tournament)
      sender ! LobbyName(tournament)
  }
}

trait ValidGame extends Actor {

  //called when a player make a move and have to return a Result if possible
  def playGame(player: String, move: String) : Option[Result]
  
  //called when the game is ready to start
  def startGame()
  
  //def createGame(player1: String, player2: String) : ValidGame
  
  def setWinner(winner: String, winmove: String, looser: String, loosemov: String) {
    context.parent ! Result((winner,winmove),(looser,loosemov))
    Chifoumi.default ! WinLost(winner, winmove, looser, loosemov, context.parent.path.name.toInt)
    context.stop(self)
  }

  
  def receive = {
    case Play(player, move) =>
      playGame(player, move).foreach { result =>
          println("result found "+result)
          setWinner(result.winner._1, result.winner._2, result.looser._1, result.looser._2)
      }
      
    case Start =>
      startGame()
  }
}

object ValidChoumi {
  def apply(player1: String, player2: String) = new ValidChoumi(player1, player2)
}

class ValidChoumi(player1: String, player2: String) extends ValidGame {

  var moves : Map[String,String] = Map.empty
	val rules = ChifoumiGame.rules
  
  override def receive = {
    // add your custom Events here 
    super.receive.orElse {
      case _ => println("unhandled")
    }
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
            startGame()
            None
          case `stronger` =>
            
            Some(Result((player,move),(lastplayer,lastmove)))
            //setWinner(player,move,lastplayer,lastmove)
          case _ => 
            Some(Result((lastplayer,lastmove),(player, move)))
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

class ValidChifoumiBo3(player1 : String, player2: String) extends ValidChoumi(player1, player2) {

  var wins : List[String] = Nil

  override def playGame(player: String, move:  String) = {
    println(player + " plays " + move)
    moves = moves + (player -> move)
    if(moves.size == 2) {
      val (lastplayer, lastmove) = (moves - player).head
      val stronger : String = rules((rules.indexOf(lastmove)+1)%3)
      move match {
        case `lastmove` => 
          //draw, replay !
          startGame()
          None
        case `stronger` =>
          if(wins.contains(player))
            Some(Result((player,move),(lastplayer,lastmove)))
          else {
            wins = wins :+ player
            startGame()
            None
          }
        case _ => 
          if(wins.contains(lastplayer))
            Some(Result((lastplayer,lastmove),(player, move)))
          else {
            wins = wins :+ lastplayer
            startGame()
            None
          }
          
      }
      
    } else None
  }
}

