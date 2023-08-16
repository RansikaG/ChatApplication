-module(chat_fsm).
-behaviour(gen_fsm).

-export([start_link/0]).
-export([init/1]).

-record(state, {clients, client_pid, name}).

start_link() ->
  gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

init(Args) ->
  Clients = proplists:get_value(clients, Args),
  Name = proplists:get_value(name, Args),
  ClientPid = proplists:get_value(client_pid, Args),
  {ok, connected, #state{clients = Clients, name = Name, client_pid = ClientPid}}.


connected({send, {RecieverName, Message}}, State) ->
    %%io:fwrite("Send ~p ~p ~n",[RecieverName, Message]),
    Clients = State#state.clients,
    SenderName = State#state.name,
    Reply =
    case proplists:get_value(RecieverName, Clients) of
        undefined ->
            {error, no_client};
        HandlerPid ->
            gen_fsm:send_event(HandlerPid, {recieve, {SenderName, Message}})
    end,
    {next_state, connected, State};

connected({recieve, {SenderName, Message}}, State) ->
    %%io:fwrite("receive ~p ~p ~n",[SenderName, Message]),
    ClientPid = State#state.client_pid,
    Reply = gen_server:call(ClientPid, {recieve, {SenderName, Message}}),
    %%ClientPid !  {recieve, SenderName, Message},
    {next_state, connected, State};

connected(_Event,  State) ->
    Reply = ok,
    {next_state, connected, State}.
