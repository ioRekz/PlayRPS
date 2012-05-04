var createDomBracket = function(players) {
	var mgtop = 8;
	var offtop = 27;
	var space = 6;
	var offspace = 54;
	var roundid = 0;
	var roundG = $('#round-template').tmpl({mgtop: mgtop, round:roundid})
	var round = roundG.find('.nodes')
	
	for(i = 0, matchid=0, dir=true; i < players.length; i=i+2, dir= !dir, matchid++){
		var game = $('#game-template').tmpl({matchid: matchid, direction: dir ? "down" : "up"})
		var opponentOne = $('#opponent-template').tmpl({name: players[i], nbOpo: "one"})
		var opponentTwo = $('#opponent-template').tmpl({name: players[i+1], nbOpo: "two"})
		var gamecontainer = game.find('.match')
		gamecontainer.append(opponentOne)
		gamecontainer.append(opponentTwo)
		round.append(game)
		var line = $('#line-template').tmpl({height: space, type: dir ? "line" : "space"})
		round.append(line)
	}
	$('.bracket').append(roundG)
	
	space = space + offspace;
	offspace = offspace * 2;
	mgtop = mgtop + offtop;
	offtop = offtop * 2
	roundid++;
	
	for(j = players.length/2; j >=2 ; j=j/2, roundid++) {
		var roundG = $('#round-template').tmpl({mgtop: mgtop, round: roundid})
		var round = roundG.find('.nodes')
			for(x = 0, dir=true; x < j/2; x++, dir = !dir) {
				var opponentOne = $('#opponent-template').tmpl({nbOpo: "one"})
				var opponentTwo = $('#opponent-template').tmpl({nbOpo: "two"})
				var gameC = $('#game-template').tmpl({matchid: x,direction: dir ? "down" : "up"});
				gameC.prepend("<div class='bracket-mark'></div>")
				var game = gameC.find('.match');
				game.append(opponentOne)
				game.append(opponentTwo)
				round.append(gameC)
				var line = $('#line-template').tmpl({height: space, type: dir ? "line" : "space"})
				round.append(line)
			}
		space = space + offspace;
		offspace = offspace * 2;
		mgtop = mgtop + offtop;
		offtop = offtop * 2
		$('.bracket').append(roundG)
	}
	$('.bracket-line:last').remove();
	$('.bracket-down:last').remove();
}

var setWinner = function(winner, looser, round) {
	var oppo = $("#round-"+round+" .opponent[data-id='"+winner.name+"']")
	oppo.addClass('winner')
	
	
	
	var gameid = parseInt(oppo.parent().attr("data-id"))
	
	var nextGameId = Math.floor((gameid)/2)
	var offsetPlayer = (gameid)%2 == 0 ? "one" : "two"

	var nextGame = $("#round-"+(round+1)+" .match[data-id='"+nextGameId+"']")

	var nextOpo = nextGame.find('.opponent.'+offsetPlayer);
	nextOpo.append($("#name-template").tmpl({name: winner.name}))
	nextOpo.attr('data-id', winner.name)
  if(winner.name==currentUser)
    scalePlayer(currentUser)
}

var scalePlayer = function(name) {
  var lastSeen = $('.opponent[data-id='+name+']').last()
  $('body').scrollTo(lastSeen, {offset: -400})
}