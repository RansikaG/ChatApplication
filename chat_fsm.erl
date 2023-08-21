-module(chat_fsm).
-behaviour(gen_statem).

-export([start_link/0]).
-export([init/1, callback_mode/0, handle_event/4]).

-record(state, {clients, client_pid, name, available_groups, subscribed_groups}).

start_link() ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

init(Args) ->
  Clients = proplists:get_value(clients, Args),
  Name = proplists:get_value(name, Args),
  ClientPid = proplists:get_value(client_pid, Args),
  AvailableGroups = proplists:get_value(groups, Args),
  {ok, connected, #state{clients = Clients, name = Name, client_pid = ClientPid, available_groups = AvailableGroups}}.

callback_mode() ->
    [handle_event_function].

handle_event(cast,{join, {Name, Pid}}, connected , State) ->
    Clients = State#state.clients,
    NewState = State#state{clients = lists:concat([Clients,[{Name, Pid}]])},
    {next_state, connected, NewState};

handle_event({call,From},{send,{RecieverName, Message}}, connected, State) ->
    io:fwrite("Send ~p ~p ~n",[RecieverName, Message]),
    Clients = State#state.clients,
    SenderName = State#state.name,
    case proplists:get_value(RecieverName, Clients) of
        undefined ->
            {error, no_client};
        HandlerPid ->
            gen_statem:call(HandlerPid, {recieve, {SenderName, Message}})
    end,
    {next_state, connected, State, {reply,From,"Message sent"}};
    
handle_event({call,From},{recieve, {SenderName, Message}},connected, State) ->
    io:fwrite("receive ~p ~p ~n",[SenderName, Message]),
    ClientPid = State#state.client_pid,
    gen_server:call(ClientPid, {recieve, {SenderName, Message}}),
    % ClientPid !  {recieve, SenderName, Message},
    {next_state, connected, State,{reply,From,"Message received"}};

handle_event(cast,{group_info, {GroupName, GroupPid}}, connected , State) ->
    ClientPid = State#state.client_pid,
    gen_server:call(ClientPid, {receive_group_info, GroupName}),
    AvailableGroups = State#state.available_groups,
    NewState = State#state{ available_groups = lists:concat([AvailableGroups, [{GroupName, GroupPid}]])},
    io:format("New State_callback_group_info:(~p) ~p .~n",[State#state.name, NewState]),
    {next_state, connected, NewState};

handle_event({call,From},{subscribe, GroupName},connected, State) ->
    GroupPid = proplists:get_value(GroupName, State#state.available_groups),
    Reply = gen_server:call(GroupPid, subscribe),
    SubscribedGroups = State#state.subscribed_groups,
    % ClientPid !  {recieve, SenderName, Message},
    NewState = State#state{subscribed_groups = lists:concat([SubscribedGroups, [{GroupName, GroupPid}]])},
    io:format("New State_callback_subscribe:(~p) ~p .~n",[State#state.name, NewState]),
    {next_state, connected, NewState,{reply,From,Reply}};

handle_event({call, From}, {send_group_msg, {GroupName, Message}}, connected, State) ->
    try
        io:format("New State_callback_send_group_msg:(~p) ~p .~n",[State#state.name, State]),
        GroupPid = proplists:get_value(GroupName, State#state.subscribed_groups),
        Reply = gen_server:call(GroupPid, {send_group_msg, {Message, State#state.name}}),
        {next_state, connected, State, {reply, From, Reply}}
    catch
        error:badarg ->
            io:format("Error: badarg~n"),
            {next_state, connected, State, {reply, From, "Bad arg"}};
        error:MatchError ->
            io:format("Error: function clause mismatch~n"),
            {next_state, connected, State, {reply, From, "Cannot send the message"}};
        _:Error ->
            io:format("Unknown error: ~p~n", [Error]),
            {next_state, connected, State, {reply, From, "Unknown Error"}}
    end;

handle_event(cast, {group_msg, {Group, Message, Sender}}, connected, State) ->
    ClientPid = State#state.client_pid,
    gen_server:call(ClientPid, {receive_group_msg, {Group, Message, Sender}}),
    io:format("New State_callback_group_msg:(~p) ~p .~n",[State#state.name, State]),
    {next_state, connected, State};

handle_event({call, _From}, {_Event, _}, connected, State) ->
    io:format("New State_callback_any:(~p) ~p .~n",[State#state.name, State]),
    {next_state, connected, State}.
    

% connected({send,{_RecieverName, _Message}}, connected, State) -> 
%     {next_state, connected, State}.
    