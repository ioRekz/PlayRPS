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
		if events[event]
			console.warn('Overriding existing event function for : '+event)
		events[event] = func
	
	send: (msg) ->
		@socket.send msg