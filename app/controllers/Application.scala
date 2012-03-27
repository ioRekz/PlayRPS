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
    Ok(views.html.index("toto",Nil))
  }
	
	def socketJoin(player : String) = WebSocket.async[JsValue] { request  =>
		Chifoumi.join(player)
	}
	
	def join(player: String) = Action { implicit request =>
		
		println("join : "+Tournament.players)
		Tournament.players = Tournament.players - player
		Ok(views.html.index(player,Tournament.players.toList))
	}
  
}