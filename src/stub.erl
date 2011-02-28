-module(stub).
-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/3, stop/2, proxy_call/3]).
-record(state, {old_code, module, stub_fun}).

%
% A funciton stub or stub is a piece of code used to stand in for some other programming functionality. 
% A stub may simulate the behaviour of existing code or be a temporary substitute for yet-to-be developed code.
%
start(Module, Function, StubFun) when is_function(StubFun) ->
  gen_server:start({local, reg_fun_name(Module, Function)}, ?MODULE, [Module, Function, StubFun], []).
  
stop(Module, Function) ->
	RegisteredAs = reg_fun_name(Module, Function),
	case lists:member(RegisteredAs, erlang:registered()) of 
		true -> 
		    gen_server:cast(RegisteredAs, stop),
			timer:sleep(10);
		false -> 
			{error, not_registered, RegisteredAs}
	end.

proxy_call(_, Name, Args) ->
  gen_server:call(Name, {proxy_call, Args}).

%-------------------------------------------------------------------
% gen_server functions
% ------------------------------------------------------------------
init([Module, Function, StubFun]) ->
	case code:get_object_code(Module) of
		{Module, Bin, Filename} ->
			try
				stub_function(Module, Function, arity(StubFun)),
				timer:sleep(10),
				{ok, #state{module=Module,old_code={Module,Bin,Filename},stub_fun=StubFun}} 
			catch
				_C:_E ->
					io:format(user, "To use stub, the module has to be compiled with +debug_info~n", []),
					io:format(user, "Backtrace ~p~n", [erlang:get_stacktrace()]),
					{error, module_without_debug_info}
			end;
		error -> 
			{error, non_existing_module, Module} 
	end.

handle_call({proxy_call, Args}, _From, State = #state{stub_fun=StubFun}) ->
  Reply = apply(StubFun, tuple_to_list(Args)),
  {reply, Reply, State}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{old_code={Module, Binary, Filename}}) ->
  code:purge(Module),
  code:delete(Module),
  code:load_binary(Module, Filename, Binary).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%-----------------------------------------------------------------------------------------
% Internal functions
%  copied/modified from http://github.com/cliffmoon/effigy(Cliff Moon/Brad Andersoon )
%-----------------------------------------------------------------------------------------
reg_fun_name(_Module, Function) ->
  list_to_atom(lists:concat([Function, "_stub"])).
  
stub_function(Module, Function, Arity) ->
  {_, Bin, _} = code:get_object_code(Module),
  {ok, {Module,[{abstract_code,{raw_abstract_v1,Forms}}]}} = beam_lib:chunks(Bin, [abstract_code]),
  StubbedForms = replace_function(Module, Function, Arity, Forms),
  case compile:forms(StubbedForms, [binary]) of
    {ok, Module, Binary} -> code:load_binary(Module, atom_to_list(Module) ++ ".erl", Binary);
    Other -> Other
  end.

arity(Fun) when is_function(Fun) ->
  Props = erlang:fun_info(Fun),
  proplists:get_value(arity, Props).

replace_function(Module, Function, Arity, Forms) ->
  replace_function(Module, Function, Arity, Forms, []).
  
replace_function(_Module, _Function, _Arity, [], Acc) ->
  lists:reverse(Acc);
replace_function(Module, Function, Arity, [{function, Line, Function, Arity, _Clauses}|Forms], Acc) ->
  lists:reverse(Acc) ++ [{function, Line, Function, Arity, [
    {clause,
      Line,
      generate_variables(Arity),
      [],
      generate_expression(stub,proxy_call,Module,reg_fun_name(Module,Function),Arity)}]}] ++ Forms;
replace_function(Module, Function, Arity, [Form|Forms], Acc) ->
  replace_function(Module, Function, Arity, Forms, [Form|Acc]).

generate_variables(0) -> [];
generate_variables(Arity) ->
  lists:map(fun(N) ->
      {var, 1, list_to_atom(lists:concat(['Arg', N]))}
    end, lists:seq(1, Arity)).
    
generate_expression(M, F, Module, Name, 0) ->
  [{call,1,{remote,1,{atom,1,M},{atom,1,F}}, [{atom,1,Module}, {atom,1,Name}, {tuple,1, []} ]}];
generate_expression(M, F, Module, Name, Arity) ->
  [{call,1,{remote,1,{atom,1,M},{atom,1,F}}, [{atom,1,Module}, {atom,1,Name}, {tuple,1,lists:map(fun(N) ->
      {var, 1, list_to_atom(lists:concat(['Arg', N]))}
    end, lists:seq(1, Arity))}]}].

-ifdef(TEST).
%-module(my_module).
%-compile(export_all).
%foo()    -> foo0.
%foo(_)   -> foo1.
%foo(_,_) -> foo2.

stub_test() ->
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
-endif.
