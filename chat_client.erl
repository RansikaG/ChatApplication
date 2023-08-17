-module(chat_client).
-behaviour(gen_server).

-export([start_link/1, send_msg/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-record(state, {handler_pid}).

start_link(Name) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [{name, Name}], []).

init(Args) ->
    State = #state{handler_pid = null},
    %%Name = proplists:get_value(name, Args), Can be used to get the name
    {value, {_, Name}} = lists:keysearch(name, 1, Args),
    io:fwrite("Name : ~p ~n", [Name]),
    case gen_server:call({global, chat_server}, {register, Name}) of
        {ok, HandlerPid} ->
            io: fwrite("fsm started for :~p with pid ~p ~n",[Name, HandlerPid]),
            {ok, State#state{ handler_pid = HandlerPid}};
        {error, Reason} ->
            io:fwrite("terminating chat_client Reason : ~p ~n", [Reason]),
        {stop, normal}
    end.

send_msg(Name, Message, OwnPID) ->
    gen_server:call(OwnPID, {send, {Name, Message}}).

handle_call({send, {Name, Message}}, _From, State) ->
    HandlerPid = State#state.handler_pid,
    io:fwrite("Saved Handler Pid: ~p~n",[HandlerPid]),
    Reply = gen_statem:call(HandlerPid, {send, {Name, Message}}),
    {reply, Reply, State};

handle_call({recieve, {Sender, Message}}, _From, State) ->
    io:fwrite("~p ~n From: ~p~n",[Message, Sender]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.