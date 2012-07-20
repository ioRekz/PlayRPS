package controllers

import play.api._
import play.api.mvc._
import models._
import models.Tournament
import play.api.libs.json._
import models._
import play.api.libs.iteratee._
import play.api.libs.Comet
import akka.actor._
import play.api.libs.concurrent._
import play.api.Play.current
import akka.util.duration._

object Application extends Controller {

	var tourney : Boolean = false
  
  def index = Action { implicit resquest =>
    Redirect(routes.Application.hello)
    
  }
	
	def socketJoin(player : String, tournament: String) = WebSocket.async[JsValue] { request  =>
		Chifoumi.join(player, tournament)
	}
	
	def join(player: String, tournament: String) = Action { implicit request =>
		
		Ok(views.html.index(player,tournament))
	}
	
	var clients : List[PushEnumerator[String]] = Nil
  
	def comet = Action {
		val enum = Enumerator.imperative[String]()
		Akka.system.scheduler.scheduleOnce(6 seconds) {
        enum.push("lol")
    }
		Ok.stream(enum &> Comet(callback = "parent.cometMessage"))
	}
	
	def hello = Action { implicit resquest =>
		Async {
			Chifoumi.getLobbies.map { lobbies =>
				Ok(views.html.hello(lobbies))
			}
		}
	}

	def sendEvent = Action {
		Chifoumi.notifyJoinQuit(LobbyInfo("test",2,5))
		Redirect(routes.Application.hello)
	}
	
	val toEventSource = Enumeratee.map[JsValue]{ msg => 
		println("data: "+msg+"\n")
		"data: "+msg+"\n\n"
	}

  def sse = Action {
		val enum = Chifoumi.createHelloEnum()
		SimpleResult(
			header = ResponseHeader(OK, Map(
				CONTENT_LENGTH -> "-1",
				CONTENT_TYPE -> "text/event-stream"
			)), 
			enum &> toEventSource);
	}

}