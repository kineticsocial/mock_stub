Erlang Mock And Stub
====================

A mock module is simulated module that mimic the behaviour of real module in controlled ways.
A funciton stub or stub is a piece of code used to stand in for some other programming functionality. 
A stub may simulate the behaviour of existing code or be a temporary substitute for yet-to-be developed code.

compile
-------
	erlc -o ebin src/*.erl
	
test
----
	erlc -DTEST +debug_info -o ebin src/*.erl test/*.erl
	erl -pa ebin -noshell -s stub test -eval "eunit:test(stub,[verbose])" -s init stop
	erl -pa ebin -noshell -s stub test -eval "eunit:test(mock,[verbose])" -s init stop

Here is a simple example, for more information, please email me at allen.kim@epicadvertising.com

Examples
	-module(my_module).
	-compile(export_all).
	foo()    -> foo0.
	foo(_)   -> foo1.
	foo(_,_) -> foo2.

	Eshell V5.7.3  (abort with ^G)
	> {ok, _Pid} = mock:start(my_module).
	> mock:expect(my_module, foo, [], mock_foo0).
	> mock:expect(my_module, 'foo/1', mock_other_foo1).
	> my_module:foo().
	mock_foo0
	> my_module:foo(1).
	mock_other_foo1
	> mock:stop(my_module).
	> my_module:foo().
	foo0

	> gen_server:start({local, my_reg_gen_server}, my_gen_server, [], []).
	> gen_server:call(my_reg_gen_server, foo).
	call_foo
	> mock:start(my_gen_server).
	> mock:expect(my_gen_server, 'handle_call/3', {reply, mock_call_foo, []}).
	> gen_server:call(my_reg_gen_server, foo).
	mock_call_foo

	> gen_event:start_link({local,my_gen_event_manager}).
	> gen_event:add_handler(my_gen_event_manager, my_gen_event, []).
	> gen_event:call(my_gen_event_manager, my_gen_event, foo).
	call_foo
	> mock:start(my_gen_event).
	> mock:expect(my_gen_event, 'handle_call/2', {ok, mock_call_foo, []}).
	> gen_event:call(my_gen_event_manager, my_gen_event, foo).
	mock_call_foo

