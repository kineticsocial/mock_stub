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

mock test example
---------------------

% -module(my_module).
% -compile(export_all).
% foo()    -> foo0.
% foo(_)   -> foo1.
% foo(_,_) -> foo2.
mock_module_test() ->
	% before mock
	foo0 = my_module:foo(),
	foo1  = my_module:foo(1),
	foo2 = my_module:foo(1,2),

	% start mock
	{ok, _Pid} = mock:start(my_module),
	ok = mock:expect(my_module, foo, [], mock_foo0),
	ok = mock:expect(my_module, foo, [1], mock_foo1),
	ok = mock:expect(my_module, foo, [1,2], mock_foo2),
	ok = mock:expect(my_module, 'foo/1', mock_other_foo1),
	ok = mock:expect(my_module, foo, fun(_,_) -> mock_other_foo2 end ),

	% after mock
	?assertEqual( mock_foo0, my_module:foo()),
	?assertEqual( mock_foo1, my_module:foo(1)),
	?assertEqual( mock_foo2, my_module:foo(1,2)),
	?assertEqual( mock_other_foo1, my_module:foo(other)),
	?assertEqual( mock_other_foo2, my_module:foo(4,5)),
	mock:stop(my_module),

	?assertEqual( foo0, my_module:foo()),
	?assertEqual( foo1, my_module:foo(1)),
	?assertEqual( foo2, my_module:foo(1,2)).

% -module(my_gen_server).
% -compile(export_all).
% -record(state, {call=0, cast=0, info=0}).
% 
% init(_Args) -> {ok, #state{}}.
% handle_call(foo, _, #state{call=Count}=State) -> {reply, foo_call, State#state{call=Count+1}}.
% handle_cast(foo, #state{cast=Count}=State) ->    {noreply, State#state{cast=Count+1}}.
% handle_info(foo, #state{info=Count}=State) ->    {noreply, State#state{info=Count+1}}.
% terminate(_Reason, _State) -> ok.  
% code_change(_OldVsn, State, _Extra) -> {ok, State}.
mock_gen_server_test()->
	gen_server:start({local, my_reg_gen_server}, my_gen_server, [], []),
	timer:sleep(10),
	% before mock
	call_foo = gen_server:call(my_reg_gen_server, foo),
	ok = gen_server:cast(my_reg_gen_server, foo),
	foo = my_reg_gen_server ! foo,

	% start mock
	{ok, _} = mock:start(my_gen_server),
	ok = mock:expect(my_gen_server, 'handle_call/3', {reply, mock_call_foo, []}),
	ok = mock:expect(my_gen_server, 'handle_cast/2', {noreply, []}),
	ok = mock:expect(my_gen_server, 'handle_info/2', {noreply, []}),

	% after mock
	?assertEqual( mock_call_foo, gen_server:call(my_reg_gen_server, foo)),
	?assertEqual( ok , gen_server:cast(my_reg_gen_server, foo)),
	?assertEqual( foo, my_reg_gen_server ! foo),
	mock:stop(my_gen_server).

% -module(my_gen_event).
% -behaviour(gen_event).
% -export([start/1, stop/0]).
% -export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).
% 
% start(Options) -> ok = gen_event:add_sup_handler(my_gen_event_manager, ?MODULE, Options),
% stop()         -> gen_event:delete_handler(my_gen_event_manager, ?MODULE, []).
% init(_Args)              -> {ok, _State=[]}.
% handle_event(foo, State) -> {ok, State}.
% handle_call(foo, State)  -> {ok, mock_call_foo, State}.
% handle_info(_, State)    -> {ok, State}.
% code_change(_, State, _) -> {ok, State}.
% terminate(_, _)          -> ok.
mock_gen_event_test() ->
	% starting of manager/adding of a handler must have been done before your test
	gen_event:start_link({local,my_gen_event_manager}),
	gen_event:add_handler(my_gen_event_manager, my_gen_event, []),

	% before mock
	call_foo = gen_event:call(my_gen_event_manager, my_gen_event, foo), 
	ok = gen_event:notify(my_gen_event_manager, foo),

	% start mock
	{ok, _Pid} = mock:start(my_gen_event),
	ok = mock:expect(my_gen_event, 'handle_call/2', {ok, mock_call_foo, []}),
	ok = mock:expect(my_gen_event, 'handle_event/2', {ok, []}),

	% after mock
	?assertEqual( mock_call_foo, gen_event:call(my_gen_event_manager, my_gen_event, foo)),
	?assertEqual( ok, gen_event:notify(my_gen_event_manager, foo)),

	% mock reverted
	mock:stop(my_gen_event),
	?assertEqual( call_foo, gen_event:call(my_gen_event_manager, my_gen_event, foo)), 
	?assertEqual( ok, gen_event:notify(my_gen_event_manager, foo)), 
	gen_event:stop(my_gen_event_manager).

stub test example
--------------------
  %-module(my_module).
  %-compile(export_all).
  %foo()    -> foo0.
  %foo(_)   -> foo1.
  %foo(_,_) -> foo2.

	foo0 = my_module:foo(),
	foo1 = my_module:foo(1),
	foo2 = my_module:foo(1,2),

	{ok, _} = stub:start(my_module, foo, fun()-> stub_foo0 end),
	timer:sleep(100),
	?assertEqual(stub_foo0, my_module:foo()),
	foo1 = my_module:foo(1),
	foo2 = my_module:foo(1,2),  
	stub:stop(my_module, foo),
	timer:sleep(100),
	?assertEqual(foo0, my_module:foo()),

	{ok, _} = stub:start(my_module, foo, fun(_, _)-> stub_foo2 end),
	timer:sleep(100),
	foo0  = my_module:foo(),      
	foo1  = my_module:foo(1),
	?assertEqual(stub_foo2, my_module:foo(1,2)), 
	stub:stop(my_module, foo),
	timer:sleep(100),
	?assertEqual(foo2, my_module:foo(1,2)).


For more information, please email me at allen.kim@epicadvertising.com
