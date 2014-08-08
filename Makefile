all: light serial_forward
	@erl \
		-pa deps/cowboy/ebin \
		-pa deps/cowlib/ebin \
		-pa deps/ezwebframe/ebin \
		-pa deps/ranch/ebin \
		-pa ebin \
		-s light_server start

light:
	@test -d deps || rebar get-deps	
	@rebar compile

serial_forward:
	@gcc src_serial_forward/serial_forward_simu.c -o serial_forward
