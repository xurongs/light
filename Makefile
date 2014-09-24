all: light serial_forward
	@erl \
		-app_cfg app_local.cfg \
		-detached \
		-pa deps/cowboy/ebin \
		-pa deps/cowlib/ebin \
		-pa deps/ezwebframe/ebin \
		-pa deps/ranch/ebin \
		-pa ebin \
		-eval 'light_server:start(8081).'
	
	@erl \
		-app_cfg app_extern.cfg \
		-pa deps/cowboy/ebin \
		-pa deps/cowlib/ebin \
		-pa deps/ezwebframe/ebin \
		-pa deps/ranch/ebin \
		-pa ebin \
		-eval 'light_server:start(8082).'
		
light:
	@test -d deps || rebar get-deps	
	@rebar compile

serial_forward:
	@gcc src_serial_forward/serial_forward_simu.c -o serial_forward
