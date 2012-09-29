$(function() {
	var refresh

	var init = function() {
		$('#followme').change(function (e) {
			if($(this).is(":checked"))
				scalePlayer(currentUser)
		})

		//$('.game').hide()
	}

	var socketz = new WSocket(wsURL);
	socketz.on("newGame", function(data) {
		$("#p1").html(data.firstPlayer);
		$("#p2").html(data.secondPlayer).hide().fadeIn("slow");
		$(".result").html("VERSUS");
		$('#moves').show()
	});

	socketz.on("tourneyStart", function(data) {
		createDomBracket(data.members)
		$('#players').hide()
		$('.game').show()
		//scalePlayer(currentUser);
	});

	socketz.on("persoresult", function(data) {
		$('.result').show();
		if (data.draw) {

			$('.result').html($('#result-template').tmpl({
				mine: data.winner.move,
				his: data.looser.move
			}));

		} else {
			var left = data.winner.name == currentUser ? data.winner.move : data.looser.move
			var right = data.winner.name != currentUser ? data.winner.move : data.looser.move
			var reshtml = $('#result-template').tmpl({
				mine: left,
				his: right
			})
			$('.result').html(reshtml);

			if(data.winner.name == currentUser) $('#p1p').append("."); 
			else $('#p2p').append(".")

			//$('.result').html("<span>"+ left + " vs " + right +"</span>")
			//$('.result').append("<br/>"+ (data.winner.name == currentUser ? "You win" : "You lost"))
		}
		$('#moves').hide();
	});

	socketz.on("result", function(data) {
		if (data.winner.name == currentUser) {
			$('#p1p').empty();
			$('#p2p').empty();
		}

		setWinner(data.winner, data.looser, data.round)
		if (currentUser == data.looser.name) console.log("you loose")

	});

	socketz.on("lost", function(data) {
		updateAgainst("You lost")
		console.log("you lost")
	});

	socketz.on("youwin", function(data) {
		$('.result').show();
		$('.result').html("Gratz, you secured 1st place in " + currentTournament + " tournament !")
	});

	socketz.on("join", function(data) {
		//dont bother me if i'm playing
		if($('#players').is(":visible"))
			$('#players').append("<li id=" + data.user + ">" + data.user + "</li>")
	});

	socketz.on("spectate", function(data) {
		$('#players').hide();
		$('.game').hide();
		$('#bracket').css('top', 'auto');
		$('#status').append("Tournament is running, you are spectating")
		$('#followsettings').hide();
		createDomBracket(data.members)
		// defaultGame(data.members)
		_.each(data.results.rounds, function(round, roundNbr) {
			_.each(round.matches, function(match) {
				setWinner(match.winner, match.looser, roundNbr)
			});
		});
	});

	socketz.on("infos", function(data) {
		console.log('infos')
		if (data.started) {
			$('#players').hide();
			$("#p1").html(currentUser);
			$("#p2").html(data.currentGame).hide().fadeIn("slow");
			$(".result").html("VERSUS");
			createDomBracket(data.members)
			// defaultGame(data.members)
			_.each(data.results.rounds, function(round, roundNbr) {
				_.each(round.matches, function(match) {
					setWinner(match.winner, match.looser, roundNbr)
				});
			});
		} else {
			console.log("not started")
			for (i in data.members)
			$('#players').append("<li id=" + data.members[i] + ">" + data.members[i] + "</li>")
		}
	});

	socketz.on("quit", function(data) {
		$('#' + data.user).remove()
		//TODO same id as tree player?
	});
	socketz.on("global", function(data) {
		alert(data.message)
	});

	var launchTimer = function(data) {
			var pc = 0;
			if (refresh) clearInterval(refresh)
			$('.progress').addClass('active');
			$('.result').hide();
			$('.progress').show();
			var co = this
			refresh = setInterval(function() {
				pc = pc + 20
				$('.bar').width(pc + "%")
				if (pc == 120) {
					clearInterval(refresh)
					$('.progress').removeClass('active');
					$('.progress').hide();
					$('.result').show();
					$('.result').html("Too slow ... You Lost")
					//if(result)
				}
			}, 1000)

		}

	var updateAgainst = function(msg) {
			$('#against').hide()
			$('#against')[0].innerText = ""
			$('#against').append(msg)
			$('#against').fadeIn('slow')
		}

		//chatSocket.onmessage = receiveEvent
		$('.btn').bind('click', function(event) {
			socketz.send(JSON.stringify({
				kind: "move",
				player: currentUser,
				move: event.currentTarget.value
			}))
			clearInterval(refresh)
			$('.progress').removeClass('active');
			$('.progress').hide();
			$('.result').html("And the winner is ...")
		});

		init();

});