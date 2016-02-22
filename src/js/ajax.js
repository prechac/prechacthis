
function loadContent(link){
	new Ajax.Request(link+"&ajax=on", {
	  method: 'get',
	  onSuccess: function(transport) {
	    $('content').update(transport.responseText);
		createDragAndDrop();
		writeLinkHere();
        downloadJoePass();
	  }
	});
}

function writeLinkHere() {
	var info_f = $('info_form');
	var uri = "./info?pattern="+$F(info_f['pattern'])
		+"&persons="+$F(info_f['persons'])
		+"&swap="+$F(info_f['swap'])
		+"&back="+$F(info_f['back']);
	$('linkhere').update("&nbsp;&nbsp;<a href='"+uri+"'>link here</a>");
}
