function connect_to_erlang(host, port, mod){
    // console.log('connect', [host,port,mod]);
    make_live_buttons();
    make_live_inputs();
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
    websocket           = new WebSocket(wsUri); 
    websocket.onopen    = onOpen;
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
	};
    };
}
// END:do

function make_live_buttons(){
    $(".live_button").each(
	function(){
	    var b=$(this);
	    var id = b.attr('id');
	    b.click(function(){
		console.log('clicked',id);
		send_json({clicked:id});
	    });
	});
}

function send_json(x){
    // console.log('send',x);
    send(JSON.stringify(x));
}

// We want the inputs to send a message when we hit CR in the input

function make_live_inputs(){
    $(".live_input").each(
	function(){
	    var e=$(this);
	    var id = e.attr('id');
            // console.log("entry",[e,id]);
	    e.keyup(function(ev){
			if(ev.keyCode==13){
			    read_entry(e, id);
			};
		    });
	    
	});
}
	
function read_entry(x, id){
    var val = x.val();
    x.val(" ");
    send_json({'entry':id, txt:val});
}
    
// browser commands

var light_state = {};

function light_click(index){
    $("#light" + index).css("color", "gray");
    if (light_state[index]) {
        send_json({off:index});
    } else {
        send_json({on:index});
    }
}

function set_light_state(states){
    for (var index in states) {
        state = "silver";
        if (states[index]) {
            state = "black"
        }
        $("#light" + index).css("color", state);
    }
}

function cmd_state(o){
    light = o.state;
    for (var index in light_state) {
        index = eval(index);
        light_state[index] = ((light >> index) & 1);
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
    min_x = 0;
    max_x = 0;
    min_y = 0;
    max_y = 0;
    for (var i = 0; i < rooms.length; i++) {
        min_x = min(min_x, rooms[i].x);
        max_x = max(max_x, rooms[i].x + rooms[i].width);
        min_y = min(min_y, rooms[i].y);
        max_y = max(max_y, rooms[i].y + rooms[i].height);
    }
    return {"x": min_x, "y": min_y, "width": max_x - min_x, "height": max_y - min_y};
}

function draw(rooms, lights, clt_rect) {
    room_rect = room_range(rooms);
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
    unit = 4;
    half = unit / 2;
    for (var i = 0; i < lights.length; i++) {
        light_id = lights[i].index;
        if (light_id >= 0) {
            name = lights[i].text;
            x = $parent.offset().left + (offset.x + lights[i].pos.x - half) * scale;
            y = $parent.offset().top + (offset.y + lights[i].pos.y - half) * scale;
            size = unit * scale;
            var obj_light = $(
                "<div id='light" + light_id + "' onclick='light_click(" + lights[i].index + ")' style='border:3px solid;position:absolute;border-radius:" + (size / 2) + "pt;left:" + x + ";top:" + y + ";width:" + size + ";height:" + size + ";'></div>");
            $parent.append(obj_light);
            light_state[light_id] = 0;
        }
        else {
            var br = $('<br />');
            $parent.append(br);
        }
    };

    set_light_state(light_state);
}
