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
	 get_node_num/1,
	 inc_node_num/1,
	 dec_node_num/1,
	 show_working_memory/0,
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
%    ets:new(am,[public,named_table,{read_concurrency,true}]),
%    ets:new(bm,[public,named_table,{read_concurrency,true}]),
    Root = runes_compile:build_root_node_only_once(),
    Dummy_top_node = runes_compile:build_dummy_top_node_only_once(),
    ets:insert(agenda,{dummy_top_node,Dummy_top_node}),
    ets:insert(agenda,{root_node,Root}),
    ets:insert(agenda,{bm_num,0}),
    ets:insert(agenda,{am_num,0}),
    ets:insert(agenda,{ctn_num,0}),
    ets:insert(agenda,{pn_num,0}),
    ets:insert(agenda,{jn_num,0}),
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

show_working_memory() ->
    io:format("All wme_refs: ~p~n",[runes_kb:get_all_wme_refs()]),
    Trs = lists:flatten(ets:match(token_store,{'$1','_'})),
    io:format("All token_refs: ~p~n",[Trs]).

get_node_num(all_nodes) ->
    Ls = lists:map(fun(Class) ->
			   get_node_num(Class)
		   end,[ctn,am,bm,jn,pn]),
    Total_num = lists:foldl(fun({_,E},Acc) ->
				    E + Acc
			    end,0,Ls),
    io:format("Numbers of all kinks of nodes: ~p~nThe total number: ~p~n"
	      , [Ls,Total_num]);
get_node_num(Class) ->
    case Class of
	ctn ->
	    get_ctn_num();
	am ->
	    get_am_num();
	bm ->
	    get_bm_num();
	jn ->
	    get_jn_num();
	pn ->
	    get_pn_num()
    end.

get_am_num()->
    case ets:lookup(agenda,am_num) of
	[{am_num,An}]->
	    {a,An};
	[] ->
	    no
    end.

get_bm_num() ->
    case ets:lookup(agenda,bm_num) of
	[{bm_num,Bn}] ->
	    {b,Bn};
	[] ->
	    no
    end.

get_ctn_num()->
    case ets:lookup(agenda,ctn_num) of
	[{ctn_num,Cn}] ->
	    {c,Cn};
	[] ->
	    no
    end.

get_jn_num() ->
    case ets:lookup(agenda,jn_num) of
	[{jn_num,Jn}] ->
	    {j,Jn};
	[] ->
	    no
    end.

get_pn_num() ->
    case ets:lookup(agenda,pn_num) of
	[{pn_num,Pn}] ->
	    {p,Pn};
	[] ->
	    no
    end.

inc_node_num(Class) ->
    case Class of
	ctn ->
	    inc_ctn_num();
	am ->
	    inc_am_num();
	bm ->
	    inc_bm_num();
	jn ->
	    inc_jn_num();
	pn ->
	    inc_pn_num()
    end.

inc_ctn_num() ->
    {c,Cn} = get_ctn_num(),
    ets:insert(agenda,{ctn_num,Cn+1}).

inc_am_num() ->
    {a,An} = get_am_num(),
    ets:insert(agenda,{am_num,An+1}).

inc_bm_num() ->
    {b,Bn} = get_bm_num(),
    ets:insert(agenda,{bm_num,Bn+1}).

inc_jn_num() ->
    {j,Jn} = get_jn_num(),
    ets:insert(agenda,{jn_num,Jn+1}).

inc_pn_num() ->
    {p,Pn} = get_pn_num(),
    ets:insert(agenda,{pn_num,Pn+1}).

dec_node_num(Class) ->
    case Class of 
	c ->
	    dec_ctn_num();
	a ->
	    dec_am_num();
	b ->
	    dec_bm_num();
	j ->
	    dec_jn_num();
	p ->
	    dec_pn_num()
    end.
dec_ctn_num() ->
    {c,Cn} = get_ctn_num(),
    ets:insert(agenda,{ctn_num,Cn-1}).

dec_am_num() ->
    {a,An} = get_am_num(),
    ets:insert(agenda,{am_num,An-1}).

dec_bm_num() ->
    {b,Bn} = get_bm_num(),
    ets:insert(agenda,{bm_num,Bn-1}).

dec_jn_num() ->
    {j,Jn} = get_jn_num(),
    ets:insert(agenda,{jn_num,Jn-1}).

dec_pn_num() ->
    {p,Pn} = get_pn_num(),
    ets:insert(agenda,{pn_num,Pn-1}).
    
