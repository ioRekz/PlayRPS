package controllers

import play.api._
import play.api.mvc._
import models._
import models.Tournament
import play.api.libs.json._
import models._

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
  
}