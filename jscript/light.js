function connect_to_erlang(host, port, mod){
	// console.log('connect', [host,port,mod]);
	var ws = 'ws://' + host + ':' + port + '/websocket/' + mod;
	start_session(ws);
}

function onClose(evt) {
	// change the color of the display when the socket closes
	// so we can see it closed
	// console.log('closed');
	document.body.style.backgroundColor='#ffb2b2';
}  
  
function onMessage(evt) {
	var json = JSON.parse(evt.data);
	do_cmds(json);
}
  
function onError(evt) { 
	// if we get an error change the color of the display so we 
	// can see we got an error
	document.body.style.backgroundColor='orange';
}  
  
function send(msg) {
	websocket.send(msg);
}
  
function start_session(wsUri){
	// console.log('start_session', wsUri);
	websocket		   = new WebSocket(wsUri); 
	websocket.onopen	= onOpen;
	websocket.onclose   = onClose;
	websocket.onmessage = onMessage; 
	websocket.onerror   = onError;
	return(false);
}  
	
function onOpen(evt) { 
	// console.log("connected");
}

// START:do 
function do_cmds(objs){
	// console.log('do_cmds', objs);
	for(var i = 0; i < objs.length; i++){
    	var o = objs[i];
    	// as a safety measure we only evaluate js that is loaded
    	if(eval("typeof("+o.cmd+")") == "function"){
    		eval(o.cmd + "(o)");
    	} else {
    		// console.log('bad_cmd', o);
    		alert("bad_command:"+o.cmd);
    	}
	}
}
// END:do
function send_json(x){
	// console.log('send',x);
	send(JSON.stringify(x));
}

// browser commands

function light_id(id){
	return "light" + id;
}

function light_obj(id){
	return $("#" + light_id(id))
}

var light_state = {};

function light_click(id){
	light_obj(id).css("color", "gray");
	if (light_state[id] == "on") {
		send_json({off:id});
	} else {
		send_json({on:id});
	}
}

function set_light_state(states){
	for (var id in states) {
        switch (states[id])
        {
            case "on":
                state = "black";
                break;
            case "off":
                state = "silver";
                break;
            default:
                state = "red";
                break;
        }
		light_obj(id).css("color", state);
	}
}

function cmd_state(o){
	var lights = o.state;
	for (var id in lights)
	{
        var state = lights[id];
        light_state[id] = state;
	}
	set_light_state(light_state);
}

function min(a, b) {
	if (a <= b)
		return a;
	return b;
}

function max(a, b) {
	if (a >= b)
		return a;
	return b;
}

function room_range(rooms) {
	var min_x = 0;
	var max_x = 0;
	var min_y = 0;
	var max_y = 0;
	for (var i = 0; i < rooms.length; i++) {
		min_x = min(min_x, rooms[i].x);
		max_x = max(max_x, rooms[i].x + rooms[i].width);
		min_y = min(min_y, rooms[i].y);
		max_y = max(max_y, rooms[i].y + rooms[i].height);
	}
	return {"x": min_x, "y": min_y, "width": max_x - min_x, "height": max_y - min_y};
}

function draw(rooms, lights, clt_rect) {
	var room_rect = room_range(rooms);
	var offset = {"x":-room_rect.x, "y":-room_rect.y};
	var scale = min(clt_rect.width / room_rect.width, clt_rect.height / room_rect.height);
	create_room("room", rooms, offset, scale);
	create_light("main", lights, offset, scale);
}

function create_room(container, rooms, offset, scale) {
	var canvas=document.getElementById(container);
	var ctx=canvas.getContext("2d");
	ctx.fillStyle='#FF0000';
	for (var i = 0; i < rooms.length; i++) {
		x = (offset.x + rooms[i].x) * scale;
		y = (offset.y + rooms[i].y) * scale;
		width = rooms[i].width * scale;
		height = rooms[i].height * scale;
		ctx.strokeRect(x, y, width, height);
	}
}

function create_light(container, lights, offset, scale) {
	var $parent = $("#" + container);
	var unit = 4;
	var half = unit / 2;
	for (var i = 0; i < lights.length; i++) {
		var lid = lights[i].index;
		var name = lights[i].text;
		x = $parent.offset().left + (offset.x + lights[i].pos.x - half) * scale;
		y = $parent.offset().top + (offset.y + lights[i].pos.y - half) * scale;
		size = unit * scale;
		var obj_light = $(
			"<div id='" + light_id(lid) + "' onclick='light_click(\"" + lid + "\")' style='border:3px solid;position:absolute;border-radius:" + (size / 2) + "pt;left:" + x + ";top:" + y + ";width:" + size + ";height:" + size + ";'></div>");
		$parent.append(obj_light);
		light_state[lid] = "off";
	}

	set_light_state(light_state);
}
