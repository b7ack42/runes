-module(runes_agenda).

-export([init/0,
	 put_pn/2,
	 get_pn/1,
	 delete_rn/1,
	 get_root/0,
	 fire_able/2]).

-compile(export_all).

init() ->
    ets:new(agenda, [public,named_table,{read_concurrency,true}]),
    Root = runes_compile:build_root_node_only_once(),
    Dummy_top_node = runes_compile:build_dummy_top_node_only_once(),
    ets:insert(agenda,{dummy_top_node,Dummy_top_node}),
    ets:insert(agenda,{root_node,Root}).

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

fire_able(Pn,State) ->
    ok.
    
    
