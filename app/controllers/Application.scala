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
		/*val l = List("jeremy","ponpon","micka","david","mike","arnaud","marek","dana")
		l.foreach{ player => Tournament.players += player}
		if(!tourney) {
		Tournament.kickStart()
		tourney = true}*/
    Ok(views.html.index("toto"))
  }
	
	def socketJoin(player : String, tournament: String) = WebSocket.async[JsValue] { request  =>
		Chifoumi.join(player, tournament)
	}
	
	def join(player: String, tournament: String) = Action { implicit request =>
		
		Ok(views.html.index(player,tournament))
	}
  
}