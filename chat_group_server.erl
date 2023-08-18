-module(chat_group_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,{members=[], group_name}).  % Initialize the record with an empty list

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).  % Use a global name

init(Args) -> 
    GroupName = proplists:get_value(group_name, Args),
    {ok, #state{members=[], group_name=GroupName}}.  % Initialize the state record properly

handle_call(subscribe, From, State) ->
    {HandlerPid, _Tag} = From,
    io:format("Adding Client: ~p....~n", [HandlerPid]),  % Print the value for debugging
    Members = State#state.members,
    NewState = State#state{members = lists:concat([Members, [HandlerPid]])},
    {reply, {ok, "Subscribed"}, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
