$(function() {
	var source = new EventSource(sseUrl);
	source.addEventListener('message', function(e) {
		var data = JSON.parse(e.data)
		console.log("msg coming "+e.data);
		// var datas = e.data.split('-');
		// var name = datas[0];
		// var nbP = datas[1];
		if($("#"+data.name).size() > 0) {
			if(data.nbPlayers == 0)
				$("#"+data.name).remove()
			$("#"+data.name).find(".nbPlayers").html(data.nbPlayers)
		}
		else $("#tablebody").append(data.html)
		
		console.log(data.name+"  "+data.nbPlayers);
	}, false);
});
