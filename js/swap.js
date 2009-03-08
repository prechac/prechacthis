createDragAndDrop();

function createDragAndDrop() {
	var info_f = $('info_form');
	var max = $F(info_f['numbers']);
	for(var i=0; i < max; i++) {
		new Draggable(
			'throw'+i,
			{
				revert: true
			}
		);
		Droppables.add(
			'throw'+i, 
			{
				hoverclass: 'hover',
				onDrop: function(el1, el2) {
					swapAndShow(el1, el2);
				}
			}
		);
	}
}

function swapAndShow(elementA, elementB) {
	var info_f = $('info_form');
	elementB.highlight();
	var posA = elementA.identify().toString().substring(5,elementA.identify().toString().length) * 1;
	var posB = elementB.identify().toString().substring(5,elementB.identify().toString().length) * 1;
	var swapedPattern = -1;
	new Ajax.Request('./swap?pattern='+$F(info_f['pattern'])+'&pos1='+posA+'&pos2='+posB, {
		method: 'get',
		onSuccess: function(transport) {
			swapedPattern = transport.responseText;
			if(swapedPattern < 0) {
				alert("not a possible Pattern");
			}else{
				var new_uri = "./info?pattern="+swapedPattern
					+"&persons="+$F(info_f['persons'])
					+"&swap="+$F(info_f['swap'])
					+"&back="+$F(info_f['back']);
				loadContent(new_uri);
			}
		}
	});
}
	


function swapAndShow_local(elementA, elementB) {
	var info_f = $('info_form');
	elementB.highlight();
	var posA = elementA.identify().toString().substring(5,elementA.identify().toString().length) * 1;
	var posB = elementB.identify().toString().substring(5,elementB.identify().toString().length) * 1;
	var swapedPattern = swapThrows($F(info_f['pattern']), posA, posB);
	if(swapedPattern == -1) {
		alert("not a possible Pattern");
	}else{	
		var new_uri = "./info.php?pattern="+swapedPattern
			+"&persons="+$F(info_f['persons'])
			+"&swap="+$F(info_f['swap'])
			+"&ajax=on";
		loadContent(new_uri);
	}
}

function swapThrows(pattern_str, posA, posB) {
	var pos1 = Math.min(posA, posB);
	var pos2 = Math.max(posA, posB);
	var str_length = pattern_str.length;
	var pattern_arr = pattern_str.substring(3,str_length - 2).split("),p(");
	var arr_length = pattern_arr.length;
	var newPattern_str = "[";
	var dist = pos2 - pos1;
	for(var i = 0; i < arr_length; i++) {
		if(i != 0) newPattern_str += ",";
		switch(i){
		case pos1:
			var Throw_arr = pattern_arr[pos2].split(",");
			var newThrow = Throw_arr[0] * 1 + dist;
			var index = Throw_arr[1] * 1;
			var newOrigen = Throw_arr[2] * 1 + dist;
			if(notValidThrow(newThrow, index)) return -1;
			newPattern_str += "p(" +newThrow.toString() + "," + Throw_arr[1] + "," + newOrigen.toString() + ")";
			break;
		case pos2:
			var Throw_arr = pattern_arr[pos1].split(",");
			var newThrow = Throw_arr[0] * 1 - dist;
			var index = Throw_arr[1] * 1;
			var newOrigen = Throw_arr[2] * 1 - dist;
			if(notValidThrow(newThrow, index)) return -1;
			newPattern_str += "p(" + newThrow.toString() + "," + Throw_arr[1] + "," + newOrigen.toString() + ")";
			break;
		default:
			newPattern_str += "p(" + pattern_arr[i] + ")";
			break;
		}
	}
	newPattern_str += "]";
	return newPattern_str;
}

function notValidThrow(throw_float, index_int) {
	if (throw_float >= 1) return false;
	if (throw_float == 0 && index_int == 0) return false;
	return true;
}