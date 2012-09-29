package models

import akka.actor._
import play.api.libs.iteratee._
import play.api.Play.current
import play.api.libs.concurrent._
import akka.util.duration._
import play.api.libs.json._

class Player(name: String, var channel : PushEnumerator[JsValue]) extends Actor {

  var currentGame : Option[ActorRef] = None
  var currentOpo : Option[String] = None

  override def preStart() = {
  }

  def receive = {
    case JoinTourney(infos) =>
      println("infoooooooooooos")
      notifyMe("infos", "user" -> Json.toJson(name), "members" -> Json.toJson(infos.players), "started" -> Json.toJson(currentGame.isDefined),
        "results" -> parseRes(infos.results), "currentGame" -> Json.toJson(currentOpo.getOrElse("none")))

    case SpecTourney(infos) =>
      println("spec!!!!!!!!!")
      notifyMe("spectate", "user" -> Json.toJson(name), "members" -> Json.toJson(infos.players),
        "results" -> parseRes(infos.results))
      
    case Registered(players) =>
      println(players)
      notifyMe("infos", "user" -> Json.toJson(name), "members" -> Json.toJson(players), "started" -> Json.toJson(currentGame.isDefined) )
     
    case NewGame(opponent, game) => 
      notifyMe("newGame", "firstPlayer" -> Json.toJson(name), "secondPlayer" -> Json.toJson(opponent))
      currentGame = Some(game)
      currentOpo = Some(opponent)

    case play @ Play(username, move) =>
      currentGame match {
        case Some(game) => game ! play
        case None => notifyMe("global", "message" -> Json.toJson("You are not playin any game"))
      }
      
    case Reconnect(channel) => 
      println("reconnnect")
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
      println("stopifucan : "+name+" "+currentGame.isDefined)
      if(!currentGame.isDefined)
        context.stop(self)
      else println("CANT kill myself, my game is running")
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
    channel.push(
      Json.toJson(
        Map(
          "kind" -> kind
        )
      ).as[JsObject] ++ Json.toJson(elems.toMap).as[JsObject]
    )
  }

}

class RandomRobot(name: String) extends Actor {
	def receive = {
		case NewGame(opponent, game) => 
			sender ! Play(name, new scala.util.Random().shuffle(ValidChoumi.rules).head) //ChifoumiGame.rules.shuffle.head)
	}
}