Erlang Mock And Stub
====================

Here is how mock works

when expect is called, 
  1. it saves the existing module to old_code
  2. build the same name module to call epic_mock:proxy_call
  3. save the expectation to reply to the call
  
when the actual module:function is called
  1. search the expectation, then reply the call

When stop is called
  1. it stores back the original modules

expect must has two arity
  1. Function and Argument
     Arguments could be a real argument or general ones, i.e 'Arity2', 'Arity3'
  2. Reply to the call, expectation
     This could be a term, or a function


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
--------
  % for a module
  {ok, _} = mock:start(fake_module),
  ok = mock:expect({fake_module, foo, []}, bar0),
  ok = mock:expect({fake_module, foo, [1]}, bar1),
  ok = mock:expect({fake_module, foo, 'Arity2'}, fun(A,B) -> A+B end),
  ok = mock:expect({fake_module, foo, 'Arity3'}, foobar),
  ok = mock:expect({fake_module, foo, [1,2,3,4]}, fun(A,B,C,D) -> A+B+C+D end),
  
  bar0 = fake_module:foo()),
  bar1 = fake_module:foo(1)),
  5 =    fake_module:foo(2,3)),
  foobar =  fake_module:foo(1,2,3)),
  10 =  fake_module:foo(1,2,3,4)),
  mock:stop(fake_module),
  
  % for a gen_server
  {ok, _} = mock:start(fake_server),
  ok = mock:expect({fake_server, init, [[]]}, {ok,[]}),
  ok = mock:expect({fake_server, handle_call, 'Arity3'}, {reply, bar, []}),
  ok = mock:expect({fake_server, handle_cast, 'Arity2'}, {noreply, []}),
  ok = mock:expect({fake_server, handle_info, 'Arity2'}, {noreply, []}),
  gen_server:start({local, fake_server}, fake_server, [], []),
  
  bar = gen_server:call(fake_server, foo),
  ok  = gen_server:cast(fake_server, foo),
  foo = fake_server ! foo,
  mock:stop(fake_server).

