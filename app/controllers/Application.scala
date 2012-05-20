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
    Async {
      Chifoumi.getGames.map ( games => Ok(views.html.hello(games)) )
    }
    
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
	
	def stream = Action {
		Ok(views.html.stream())
	}
	
	val toEventSource = Enumeratee.map[String]{ msg => 
		"data: "+msg+"\n\n"}

 val getHeap : Enumerator[String] = Enumerator("kiki")

  def sse = Action {
		val enum = Enumerator.imperative[String]()
			Akka.system.scheduler.scheduleOnce(6 seconds) {
        enum.push("lol")
				Akka.system.scheduler.scheduleOnce(2 seconds) {
					enum.push("lol")
				}
			}
   SimpleResult(
        header = ResponseHeader(OK, Map(
          CONTENT_LENGTH -> "-1",
          CONTENT_TYPE -> "text/event-stream"
        )), 
        enum &> toEventSource);
		}
		
	
	
}