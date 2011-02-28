-module(my_gen_event).
-behaviour(gen_event).
-export([start/1, stop/0]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

start(Options) -> ok = gen_event:add_sup_handler(my_gen_event_manager, ?MODULE, Options).
stop()         -> gen_event:delete_handler(my_gen_event_manager, ?MODULE, []).
init(_Args)              -> {ok, _State=[]}.
handle_event(foo, State) -> {ok, State}.
handle_call(foo, State)  -> {ok, call_foo, State}.
handle_info(_, State)    -> {ok, State}.
code_change(_, State, _) -> {ok, State}.
terminate(_, _)          -> ok.
