-module(chat_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,{clients=[], groups=[]}).  % Initialize the record with an empty list

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).  % Use a global name

init([]) -> {ok, #state{clients=[]}}.  % Initialize the state record properly

handle_call({register, Name}, From, State) ->
    {ClientPid, _Tag} = From,
    io:format("Registering Client: ~p....~n", [Name]),  % Print the value for debugging
    Clients = State#state.clients,
    Groups = State#state.groups,
    case gen_statem:start_link(chat_fsm, [{clients, Clients}, {name, Name}, {client_pid, ClientPid}, {groups, Groups}], []) of
        {ok, Pid} ->
            NewState = State#state{clients = lists:concat([Clients, [{Name, Pid}]])},
            lists:foreach(fun({_UserName, UserPid}) -> gen_statem:cast(UserPid, {join, {Name, Pid}}) end, Clients),
            {reply, {ok, Pid}, NewState};
        {error, Reason} ->
            io:fwrite("gen_fsm start_link fail Reason : ~p ~n", [Reason]),
            {stop, normal, State}
    end;

handle_call({create_group, GroupName}, _From, State) ->
    io:format("Creating Group: ~p .....~n",[GroupName]),
    Groups = State#state.groups,
    Clients = State#state.clients,
    case gen_server:start_link(chat_group_server, [{group_name,GroupName}], []) of
        {ok, GroupPid} ->
            NewState = State#state{groups = lists:concat([Groups, [{GroupName, GroupPid}]])},
            lists:foreach(fun({_UserName, UserPid}) -> gen_statem:cast(UserPid, {group_info, {GroupName, GroupPid}}) end, Clients),
            {reply, {ok, GroupPid}, NewState};
        {error, Reason} ->
            io:fwrite("group_gen_server start_link fail Reason : ~p ~n", [Reason]),
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
