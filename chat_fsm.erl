-module(chat_fsm).
-behaviour(gen_statem).

-export([start_link/0]).
-export([init/1, callback_mode/0, handle_event/4]).

-record(state, {clients, client_pid, name}).

start_link() ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

init(Args) ->
  Clients = proplists:get_value(clients, Args),
  Name = proplists:get_value(name, Args),
  ClientPid = proplists:get_value(client_pid, Args),
  {ok, connected, #state{clients = Clients, name = Name, client_pid = ClientPid}}.

callback_mode() ->
    [handle_event_function].

handle_event(cast,{join, {Name, Pid}}, connected , State) ->
    NewState = #state{ client_pid=State#state.client_pid, name = State#state.name, clients = lists:concat([State#state.clients, [{Name, Pid}]])},
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

handle_event({call,_From},{_Event, _},connected, State) ->
    {next_state, connected, State}.

% connected({send,{_RecieverName, _Message}}, connected, State) -> 
%     {next_state, connected, State}.
    