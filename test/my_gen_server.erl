-module(my_gen_server).
-compile(export_all).
-record(state, {call=0, cast=0, info=0}).

init(_Args) -> {ok, #state{}}.
handle_call(foo, _, #state{call=Count}=State) -> {reply, call_foo, State#state{call=Count+1}}.
handle_cast(foo, #state{cast=Count}=State) ->    {noreply, State#state{cast=Count+1}}.
handle_info(foo, #state{info=Count}=State) ->    {noreply, State#state{info=Count+1}}.
terminate(_Reason, _State) -> ok.  
code_change(_OldVsn, State, _Extra) -> {ok, State}.
