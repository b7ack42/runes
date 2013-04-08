-module(runes_agenda).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-export([start/0,
	 get_conflict_set/0,
	 put_pn/2,
	 get_pn/1,
	 delete_rn/1,
	 get_root/0,
	 fire_able/2]).

-compile(export_all).

-behaviour(gen_server).


-record(agenda, {conflict_set}).

start() ->
    {ok,Agenda} = gen_server:start_link(?MODULE,[],[]),
    register(agenda,Agenda).

fire_able(Pn,_State) ->
    gen_server:cast(agenda,{fa,Pn}).

get_conflict_set() ->
    gen_server:call(agenda,get_cs).


init([]) ->
    ets:new(agenda, [public,named_table,{read_concurrency,true}]),
    Root = runes_compile:build_root_node_only_once(),
    Dummy_top_node = runes_compile:build_dummy_top_node_only_once(),
    ets:insert(agenda,{dummy_top_node,Dummy_top_node}),
    ets:insert(agenda,{root_node,Root}),
    {ok,#agenda{conflict_set=[]}}.

handle_cast({fa,Pn},State) ->
    Cset0 = State#agenda.conflict_set,
    Cset1 = [Pn|Cset0],
    {noreply,#agenda{conflict_set=Cset1}}.

handle_call(get_cs,_From,State) ->
    Set= State#agenda.conflict_set,
    io:format("the length of conflict_set: ~p~n", [length(Set)]),
    {reply,{ok,Set},State}.

handle_info(_Msg,State) ->
    {noreply,State}.

terminate(_Reason,_State) ->
    ok.

code_change(_OldVsn,State,_Extra) ->
    {ok,State}.

    
get_root() ->
    case ets:lookup(agenda,root_node) of
	[{root_node,Root}] ->
	    {ok,Root};
	[] ->
	    no_root
    end.

get_dummy_top_node() ->
    case ets:lookup(agenda,dummy_top_node) of
	[{dummy_top_node,Dn}] ->
	    {ok,Dn};
	[] ->
	    no_dummy_top_node
    end.

put_pn(Rn,Pn) ->
    ets:insert(agenda,{Rn,Pn}).

get_pn(Rn) ->
    case ets:lookup(agenda,Rn) of
	[{Rn,Pn}] ->
	    Pn;
	[] -> no_pn
    end.

delete_rn(Rn) ->
    ets:delete(agenda,Rn).

    
