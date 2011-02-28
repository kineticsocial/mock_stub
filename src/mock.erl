-module(mock).
-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-compile(export_all).
-record(state, {old_code, module, expectations=[]}).

%
% A mock module is simulated module that mimic the behaviour of real module in controlled ways.
%
start(Module) ->
	gen_server:start_link({local, reg_name(Module)}, mock, Module, []).

expect(Module, Function, Args, Expectation) ->
	gen_server:call(reg_name(Module), {expect, Function, Args, Expectation}).

expect(Module, Function, CallbackFun) when is_function(CallbackFun) ->
	Props = erlang:fun_info(CallbackFun),
	Arity = proplists:get_value(arity, Props),
	FunArity = list_to_atom(lists:concat([Function, '/', Arity])),
	gen_server:call(reg_name(Module), {expect, FunArity, CallbackFun});
	
expect(Module, FunArity, Expectation) ->
	gen_server:call(reg_name(Module), {expect, FunArity, Expectation}).

stop(Module) ->
	MockModule = reg_name(Module),
	case lists:member(MockModule, erlang:registered()) of 
		true ->
			%shoud not stop immediately because aysnc. messages might have not been processed
			timer:sleep(20),  
		    gen_server:cast(MockModule, stop),
			timer:sleep(20);  
		false -> 
			{error, not_registered, MockModule} 
	end.

proxy_call(Module, Function) ->
	gen_server:call(reg_name(Module), {proxy_call, Function, {}}).

proxy_call(Module, Function, Args) ->
	gen_server:call(reg_name(Module), {proxy_call, Function, Args}).

%-----------------------------------------------------------------------------------
% gen_server calls
%----------------------------------------------------------------------------------
init(Module) ->
  case code:get_object_code(Module) of
    {Module, Bin, Filename} ->
      case replace_code(Module) of
        ok -> 
			%wait for pid name registeration
			timer:sleep(20),
			{ok, #state{module=Module,old_code={Module, Bin, Filename}}};
        {error, Reason} -> 
			{stop, Reason}
      end;
    error -> 
		{error, non_existing_module, Module} 
  end.

handle_call({proxy_call, Function, Args}, _From, State) ->
	Reply = case get_expectation(Function, Args, State#state.expectations) of
		undefined -> 
			{error, undefined_expectations, Function, Args, State#state.expectations};
		Expectation -> 
			Expectation
	end,
	{reply, Reply, State};

handle_call({expect, Function, Args, Expectation}, _From, State) ->
	NewExpectations = set_expectation({Function,Args}, Expectation, State#state.expectations),
	{reply, ok, State#state{expectations=NewExpectations}};

handle_call({expect, FunArgs, Expectation}, _From, State) ->
	NewExpectations = set_expectation(FunArgs, Expectation, State#state.expectations),
	{reply, ok, State#state{expectations=NewExpectations}};

handle_call(_Request, _From, State) ->
	{reply, {error, ignored}, State}.

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

%-----------------------------------------------------------------------------------
% private/internal functions
%----------------------------------------------------------------------------------
reg_name(Module) when is_atom(Module) ->
	list_to_atom(lists:concat([mock_, Module])).

%
% find exact fun/args match
% if not found, found fun/arity match
% 	if it is a function, return after applying function
% 	if not, return as it is
%
get_expectation(Function, Args, Expectations) ->
	Reply = case proplists:get_value( {Function, Args}, Expectations ) of
		undefined ->
			FunArity = list_to_atom(lists:concat([Function, '/', size(Args)])),
	io:format("FunArity ~p~n",[FunArity]),
			case proplists:get_value( FunArity, Expectations ) of 
				CallbackFun when is_function(CallbackFun) ->
					Applied = apply(CallbackFun, tuple_to_list(Args)),
					Applied;
				Match ->
					Match
			end;
		Match ->
			Match
	end,
	io:format("Function ~p~n",[Function]),
	io:format("Args ~p~n",[Args]),
	io:format("Reply ~p~n",[Reply]),
	io:format("Expectations ~p~n",[Expectations]),
	case check_expectation_pattern(Function, Reply) of   % check if OTP calls return proper response
		pass ->
			Reply;
		fail ->
			{error, "invalid response", Reply, "for function", Function}
	end.

check_expectation_pattern(Function, Reply) ->
	case lists:member(Function, [handle_call, handle_cast, handle_info, handle_event, handle_sync_event]) of
		true ->
			case is_tuple(Reply) of
				true ->
					FirstElement = lists:nth(1,tuple_to_list(Reply)),
					if 
						% gen_server calls
						Function == handle_call, size(Reply) == 3, FirstElement == reply   -> pass;
						Function == handle_cast, size(Reply) == 2, FirstElement == noreply -> pass;
						Function == handle_info, size(Reply) == 2, FirstElement == noreply -> pass;
						% gen_fsm calls
						Function == handle_event, size(Reply) == 3, FirstElement == next_state -> pass;
						Function == handle_event, size(Reply) == 3, FirstElement == stop -> pass;
						Function == handle_sync_event, size(Reply) == 3, FirstElement == next_state -> pass;
						Function == handle_sync_event, size(Reply) == 4, FirstElement == reply -> pass;
						Function == handle_sync_event, size(Reply) == 3, FirstElement == stop -> pass;
						Function == handle_info, size(Reply) == 3, FirstElement == next_state -> pass;
						Function == handle_info, size(Reply) == 3, FirstElement == stop -> pass;
						% gen_event handler calls
						% TODO -- differentiate the following from gen_server calls
						Function == handle_call, size(Reply) == 3, FirstElement == ok -> pass;
						Function == handle_event, size(Reply) == 2, FirstElement == ok -> pass;
						Function == handle_info, size(Reply) == 2, FirstElement == ok -> pass;
						true -> fail
					end;
				false ->
					fail
			end;
		false -> % if not OTP function
			pass
	end.

set_expectation(FunArgs, Expectation, Expectations) ->
	NewExpectation = case FunArgs of
		{Function, Args} when is_atom(Function), is_list(Args) ->  % [arg1, arg2]
			[{{Function, erlang:list_to_tuple(Args)}, Expectation}];
		{Function, Args} when is_atom(Function), is_tuple(Args) -> % {arg2, arg2}
			[{{Function, Args}, Expectation}];
		FunctionArity    when is_atom(FunctionArity) -> % i.e. 'myfun/0'
			[{FunctionArity, Expectation}];
		_Any ->
    		error_logger:warning_report(["Invalid expectation set, will be ignored", FunArgs]),
			[]
	end,
	Expectations ++ NewExpectation.

%
% the following functions are copied from http://github.com/cliffmoon/effigy ( Author, Cliff Moon/Brad Andersoon )
%
replace_code(Module) ->
  Info = Module:module_info(),
  Exports = get_exports(Info),
  unload_code(Module),
  NewFunctions = generate_functions(Module, Exports),
  Forms = [
    {attribute,1,module,Module},
    {attribute,2,export,Exports}
  ] ++ NewFunctions,
  case compile:forms(Forms, [binary]) of
    {ok, Module, Binary} -> case code:load_binary(Module, atom_to_list(Module) ++ ".erl", Binary) of
      {module, Module} -> ok;
      {error, Reason} -> {error, Reason}
    end;
    error -> {error, "An undefined error happened when compiling."};
    {error, Errors, Warnings} -> {error, Errors ++ Warnings}
  end.

unload_code(Module) ->
  code:purge(Module),
  code:delete(Module).

get_exports(Info) ->
  get_exports(Info, []).

get_exports(Info, Acc) ->
  case lists:keytake(exports, 1, Info) of
    {value, {exports, Exports}, ModInfo} ->
      get_exports(ModInfo, Acc ++ lists:filter(fun({module_info, _}) -> false; (_) -> true end, Exports));
    _ -> Acc
  end.

generate_functions(Module, Exports) ->
  generate_functions(Module, Exports, []).

generate_functions(_Module, [], FunctionForms) ->
  lists:reverse(FunctionForms);
generate_functions(Module, [{Name,Arity}|Exports], FunctionForms) ->
  generate_functions(Module, Exports, [generate_function(Module, Name, Arity)|FunctionForms]).

generate_function(Module, Name, Arity) ->
  {function, 1, Name, Arity, [{clause, 1, generate_variables(Arity), [], generate_expression(mock, proxy_call, Module, Name, Arity)}]}.

generate_variables(0) -> [];
generate_variables(Arity) ->
  lists:map(fun(N) ->
      {var, 1, list_to_atom(lists:concat(['Arg', N]))}
    end, lists:seq(1, Arity)).

generate_expression(M, F, Module, Name, 0) ->
  [{call,1,{remote,1,{atom,1,M},{atom,1,F}}, [{atom,1,Module}, {atom,1,Name}]}];
generate_expression(M, F, Module, Name, Arity) ->
  [{call,1,{remote,1,{atom,1,M},{atom,1,F}}, [{atom,1,Module}, {atom,1,Name}, {tuple,1,lists:map(fun(N) ->
      {var, 1, list_to_atom(lists:concat(['Arg', N]))}
    end, lists:seq(1, Arity))}]}].

-ifdef(TEST).
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
-endif.
