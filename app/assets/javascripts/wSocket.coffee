class window.WSocket
	events = {}
	constructor: (@url) ->
		WS = window['MozWebSocket'] ? MozWebSocket : WebSocket
		@socket = new WebSocket(@url)
		
		@socket.onmessage = (event) ->
			data = JSON.parse(event.data)
			if(events[data.kind])
				events[data.kind](data)
    
	on: (event, func) ->
		events[event] = func
	
	send: (msg) ->
		@socket.send msg
    