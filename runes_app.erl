-module(runes_app).

-behaviour(application).

-export([start/2,stop/1]).

-define(WAIT_FOR_RESOURCES,2500).

start(_StartType, _StartArgs) ->
    ok = ensure_contact(),
    resource_discovery:add_local_resource(runes,node()),
    resource_discovery:add_target_resource_type(runes),
    resource_discovery:trade_resources(),
    timer:sleep(?WAIT_FOR_RESOURCES),
    runes_kb:init(),
    io:format("success"),
    case runes_sup:start_link() of
	{ok,Pid} ->
	    io:format("success"),
	    runes_agenda:start(),
	    {ok, Pid};
	Other->
	    {error,Other}
    end.

stop(_State) ->
    ok.

ensure_contact() ->
    DefaultNodes = ['contact@202.38.95.139'],
    case get_env(runes, contact_nodes, DefaultNodes) of
        [] ->
            {error, no_contact_nodes};
        ContactNodes ->
            ensure_contact(ContactNodes)
    end.

ensure_contact(ContactNodes) ->
    Answering = [N || N <- ContactNodes, net_adm:ping(N) =:= pong],
    case Answering of
        [] ->
            {error, no_contact_nodes_reachable};
        _ ->
            DefaultTime = 3000,
            WaitTime = get_env(runes, wait_time, DefaultTime),
            wait_for_nodes(length(Answering), WaitTime)
    end.

wait_for_nodes(MinNodes, WaitTime) ->
    Slices = 10,
    SliceTime = round(WaitTime/Slices),
    wait_for_nodes(MinNodes, SliceTime, Slices).

wait_for_nodes(_MinNodes, _SliceTime, 0) ->
    ok;
wait_for_nodes(MinNodes, SliceTime, Iterations) ->
    case length(nodes()) > MinNodes of
        true ->
          ok;
        false ->
            timer:sleep(SliceTime),
            wait_for_nodes(MinNodes, SliceTime, Iterations - 1)
    end.

get_env(AppName, Key, Default) ->
    case application:get_env(AppName, Key) of
        undefined   -> Default;
        {ok, Value} -> Value
    end.
