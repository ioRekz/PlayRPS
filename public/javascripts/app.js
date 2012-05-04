$(function() {
			var refresh

			var socketz = new WSocket(wsURL);
			socketz.on("newGame", function(data) {
				$("#p1").html(data.firstPlayer);
				$("#p2").html(data.secondPlayer).hide().fadeIn("slow");
				$(".result").html("VERSUS");
			});
			
			socketz.on("tourneyStart", function(data) {
      console.log("tourneystart");
				createDomBracket(data.members)
				$('#players').hide()
			});
      
      socketz.on("persoresult", function(data) {
        $('.result').show();
        if(data.draw) {
          $('.result').html("<span>"+ data.winner.move + " vs " + data.looser.move +"</span>");
          $('.result').append("<br/>"+"Draw");
          }
        else {
          var left = data.winner.name == currentUser ? data.winner.move : data.looser.move
          var right = data.winner.name != currentUser ? data.winner.move : data.looser.move
          $('.result').html("<span>"+ left + " vs " + right +"</span>")
          $('.result').append("<br/>"+ (data.winner.name == currentUser ? "You win" : "You lost"))
        }
      });
			
			socketz.on("result", function(data) {

        
        setWinner(data.winner, data.looser, data.round)
        
        
			});

			socketz.on("lost", function(data) {
				updateAgainst("You lost")
			});

			socketz.on("youwin", function(data) {
				$('.result').show();
				$('.result').html("Gratz, you secured 1st place in @tournament tournament !")
			});

			socketz.on("join", function(data) {
				$('#players').append("<li id="+data.user+">"+data.user+"</li>")
			});
			
			socketz.on("infos", function(data) {
				if(data.started) {
					console.log(data.results)
          $("#p1").html(currentUser);
          $("#p2").html(data.currentGame).hide().fadeIn("slow");
          $(".result").html("VERSUSS");
					createDomBracket(data.members)
					// defaultGame(data.members)
					_.each(data.results.rounds, function(round, roundNbr) {
						_.each(round.matches, function(match) {
							setWinner(match.winner, match.looser, roundNbr)
						});
					});
				 }
				else {
				for(i in data.members)
					$('#players').append("<li id="+data.members[i]+">"+data.members[i]+"</li>")
				} 
			});

			socketz.on("tourneyStart", function(data) {
				createDomBracket(data.members)
				$('#players').hide()
			});
			socketz.on("quit", function(data) {
				$('#'+data.user).remove()
				//TODO same id as tree player?
			});
			socketz.on("global", function(data) {
				alert(data.message)
			});

			var launchTimer = function(data) {
				var pc = 0;
				if(refresh)
				clearInterval(refresh)
				$('.progress').addClass('active');
				$('.result').hide();  
				$('.progress').show();
				var co = this
				refresh = setInterval(function() { 
					pc = pc + 20
					$('.bar').width(pc + "%")
					if(pc == 120) {
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
				socketz.send(JSON.stringify({kind: "move", player: currentUser, move: event.currentTarget.value}))
				clearInterval(refresh)
				$('.progress').removeClass('active');
				$('.progress').hide();
				$('.result').html("And the winner is ...")
			});
		});