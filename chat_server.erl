-module(chat_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,{clients=[]}).  % Initialize the record with an empty list

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).  % Use a global name

init([]) -> {ok, #state{clients=[]}}.  % Initialize the state record properly

handle_call({register, Name}, From, State) ->
    {ClientPid, _Tag} = From,
    io:format("The value is: ~p.~n", [From]),  % Print the value for debugging
    Clients = State#state.clients,
    case gen_fsm:start_link(chat_fsm, [{clients, Clients}, {name, Name}, {client_pid, ClientPid}], []) of
        {ok, Pid} ->
            NewState = State#state{ clients = lists:concat([Clients, [{Name, Pid}]])},
            lists:foreach(fun({_UserName, UserPid}) -> gen_fsm:send_all_state_event(UserPid, {join, {Name, Pid}}) end, Clients),
            {reply, {ok, Pid}, NewState};
        {error, Reason} ->
            io:fwrite("gen_fsm start_link fail Reason : ~p ~n", [Reason]),
            {stop, normal, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
