%
% Here is how it works
%
% when expect is called, 
%   1. it saves the existing module to old_code
%   2. build the same name module to call epic_mock:proxy_call
%   3. save the expectation to reply to the call
%   
% when the actual module:function is called
%   1. search the expectation, then reply the call
%
% When stop is called
%   1. it stores back the original modules
%
% expect must has two arity
%   1. Function and Argument
%      Arguments could be a real argument or general ones, i.e 'Arity2', 'Arity3'
%   2. Reply to the call, expectation
%      This could be a term, or a function
%
-module(mock).
-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-compile(export_all).
-record(state, { old_code, module, forms, expectations}).

start(Module) ->
	gen_server:start_link({local, reg_name(Module)}, mock, Module, []).

expect({Module, Function, Args}, Expectation) ->
	gen_server:call(reg_name(Module), {expect, Function, Args, Expectation}).

stop(Module) ->
	timer:sleep(30),  % in case gen_server messages are not fully processed. i.e. $gen_cast
	case lists:member(reg_name(Module), erlang:registered()) of 
		true -> gen_server:cast(reg_name(Module), stop);
		false-> {error, not_registered, reg_name(Module)} 
	end.

proxy_call(Module, Function, Args) ->
	gen_server:call(reg_name(Module), {proxy_call, Function, Args}).

%-----------------------------------------------------------------------------------
% gen_server calls
%----------------------------------------------------------------------------------
init(Module) ->
	OldCode = code:get_object_code(Module),
	State = #state{
		old_code=OldCode, 
		module=Module, 
		forms=[ {attribute,1,module,Module},{attribute,2,export,[]} ],
		expectations=[]},
	{ok, State}.

handle_call({proxy_call, Function, TupleArgs}, _From, State) ->
	Args = tuple_to_list(TupleArgs),
	Reply = case get_expectation(Function, Args, State#state.expectations) of
		undefined -> 
			{error, undefined_expectations, Function, Args, State#state.expectations};
		Expectation -> 
			Expectation
	end,
	{reply, Reply, State};

handle_call({expect, Fun, Args, Expectation}, _From, State) ->
	true = is_valid_expectation(Fun, Args, Expectation),
	NewExpectations = new_expectations(State#state.expectations, {Fun, Args}, Expectation),
	NewForms        = new_forms(State, {Fun, Args}),

	% delete existing module
	code:purge(State#state.module),
	code:delete(State#state.module),

	% then, activate a new module to call mock:proxy_call all the time
	{ok, Module, Binary} = compile:forms(NewForms, [binary]),
	{module, Module} = code:load_binary( Module, atom_to_list(Module) ++ ".erl", Binary ),

	NewState = State#state{
		forms=NewForms, 
		expectations=NewExpectations },
	{reply, ok, NewState} ;

handle_call(_Request, _From, State) ->
	{reply, {error, ignored}, State}.

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
	code:purge(State#state.module),
	code:delete(State#state.module),
	case State#state.old_code of
		{Module, Binary, Filename} ->
			code:load_binary(Module, Filename, Binary);
		_ ->
			ok
	end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%-----------------------------------------------------------------------------------
% private/internal functions
%----------------------------------------------------------------------------------
reg_name(Module) when is_atom(Module) ->
	list_to_atom(lists:concat([mock_, Module])).

new_expectations(ExistingExpectations, {Fun,Args}, Expectation) -> 
	NewExpectations = case lists:keyfind({Fun,Args}, 1, ExistingExpectations) of
		{{Fun,Args}, _Value} -> % expectation already registered?, set as new one
			lists:keyreplace({Fun,Args}, 1,ExistingExpectations,{{Fun,Args},Expectation});
		false ->        % new expectation?
			ExistingExpectations ++ [{{Fun,Args},Expectation}]
	end,
	NewExpectations.

new_forms(State,  {Function, Args}) ->
	Module = State#state.module,
	[{attribute,1,module,Module},{attribute,2,export,Exports}|_] = State#state.forms,
	NewExports = if 
		is_atom(Args) -> % i.e 'Arity2'
			{match, [_,Arity]} = re:run(atom_to_list(Args),"Arity([0-9]+)",[{capture, all, list}]),
			Exports ++ [{Function, list_to_integer(Arity)}];
		is_list(Args) ->
			Exports ++ [{Function, erlang:length(Args)}] 
	end,
	ProxyCallFunctions = generate_functions(Module, NewExports),
	NewForms = [ 
		{attribute,1,module,Module}, 
		{attribute,2,export, lists:usort(NewExports)} ] 
		++ lists:usort(ProxyCallFunctions),
	NewForms.

get_expectation(Function, Args, Expectations) ->
	ArgArity = list_to_atom("Arity"++integer_to_list(erlang:length(Args))),
	Reply = case lists:keyfind( {Function,Args}, 1, Expectations ) of  % find exact match first
		{_, Expectation} ->
			Expectation;
		false ->
			case lists:keyfind( {Function,ArgArity}, 1, Expectations) of  % find arity match second
				{_, Value} -> Value;
				false      -> {error, undefined_expectation} 
			end 
	end,
	if   % apply function if necessary as result
		is_function(Reply) -> apply(Reply, Args);
		true               -> Reply
	end.

is_valid_expectation(Function, _Args, Reply) ->
	IsOTPFunction = lists:member(Function, [handle_call, handle_cast, handle_info, handle_event, handle_sync_event]), 
	if 
		IsOTPFunction == true ->
			case is_tuple(Reply) of
				true ->
					FirstElement = lists:nth(1,tuple_to_list(Reply)),
					if 
						% gen_server calls
						Function == handle_call, size(Reply) == 3, FirstElement == reply   -> true;
						Function == handle_cast, size(Reply) == 2, FirstElement == noreply -> true;
						Function == handle_info, size(Reply) == 2, FirstElement == noreply -> true;
						% gen_fsm calls
						Function == handle_event, size(Reply) == 3, FirstElement == next_state -> true;
						Function == handle_event, size(Reply) == 3, FirstElement == stop -> true;
						Function == handle_sync_event, size(Reply) == 3, FirstElement == next_state -> true;
						Function == handle_sync_event, size(Reply) == 4, FirstElement == reply -> true;
						Function == handle_sync_event, size(Reply) == 3, FirstElement == stop -> true;
						Function == handle_info, size(Reply) == 3, FirstElement == next_state -> true;
						Function == handle_info, size(Reply) == 3, FirstElement == stop -> true;
						% gen_event handler calls
						% TODO -- differentiate the following from gen_server calls
						Function == handle_call, size(Reply) == 3, FirstElement == ok -> true;
						Function == handle_event, size(Reply) == 2, FirstElement == ok -> true;
						Function == handle_info, size(Reply) == 2, FirstElement == ok -> true;
						true -> false
					end;
				false ->
					false
			end;
		true -> % if not OTP function
			true
	end.

generate_functions(Module, Exports) ->
	generate_functions(Module, Exports, []).

generate_functions(_Module, [], FunctionForms) ->
	lists:reverse(FunctionForms);
generate_functions(Module, [{Name,Arity}|Exports], FunctionForms) ->
	generate_functions(Module, Exports, [generate_function(Module, Name, Arity)|FunctionForms]).

generate_function(Module, Fun, Arity) ->
	{function, 1, Fun, Arity, [{clause, 1, generate_variables(Arity), [], generate_expression(?MODULE, proxy_call, Module, Fun, Arity)}]}.

generate_variables(0) -> [];
generate_variables(Arity) ->
	lists:map(fun(N) -> 
			{var, 1, list_to_atom(lists:concat(['Arg', N]))} 
	end, lists:seq(1, Arity)).

generate_expression(M, F, Module, Name, Arity) ->
	[{call,1,{remote,1,{atom,1,M},{atom,1,F}}, [{atom,1,Module}, {atom,1,Name}, {tuple,1, generate_variables(Arity)} ]}].

-ifdef(TEST).

module_test() ->
	{ok, _Pid} = mock:start(fake_module),
	ok = mock:expect({fake_module, foo, []}, bar0),
	ok = mock:expect({fake_module, foo, [1]}, bar1),
	ok = mock:expect({fake_module, foo, 'Arity2'}, fun(A,B) -> A+B end),
	ok = mock:expect({fake_module, foo, 'Arity3'}, foobar),
	ok = mock:expect({fake_module, foo, [1,2,3,4]}, fun(A,B,C,D) -> A+B+C+D end),

	?assertEqual( bar0, fake_module:foo()),
	?assertEqual( bar1, fake_module:foo(1)),
	?assertEqual( 5,    fake_module:foo(2,3)),
	?assertEqual( foobar, fake_module:foo(1,2,3)),
	?assertEqual( 10,   fake_module:foo(1,2,3,4)),
	mock:stop(fake_module).

gen_server_test()->
	{ok, _Pid} = mock:start(fake_server),
	ok = mock:expect({fake_server, init, [[]]}, {ok,[]}),
	ok = mock:expect({fake_server, handle_call, 'Arity3'}, {reply, bar, []}),
	ok = mock:expect({fake_server, handle_cast, 'Arity2'}, {noreply, []}),
	ok = mock:expect({fake_server, handle_info, 'Arity2'}, {noreply, []}),
	_Result = gen_server:start({local, fake_server}, fake_server, [], []),

	?assertEqual( bar, gen_server:call(fake_server, foo)),
	?assertEqual( ok , gen_server:cast(fake_server, foo)),
	?assertEqual( foo, fake_server ! foo),
	mock:stop(fake_server).

mock_gen_event_test() ->
	% start mock
	{ok, _Pid} = mock:start(fake_event),
	ok = mock:expect({fake_event, init,  'Arity1'}, {ok, []}),
	ok = mock:expect({fake_event, handle_call,  'Arity2'}, {ok, bar, []}),
	ok = mock:expect({fake_event, handle_event, 'Arity2'}, {ok,[]}),

	% start gen_event
	_Result1 = gen_event:start_link({local,fake_event_manager}),
	_Result2 = gen_event:add_handler(fake_event_manager, fake_event, []),

	% call gen_event
	?assertEqual( bar, gen_event:call(fake_event_manager, fake_event, foo)),
	?assertEqual( ok,  gen_event:notify(fake_event_manager, foo)),
	
	% stop
	gen_event:stop(fake_event_manager),
	mock:stop(fake_event).

-endif.
