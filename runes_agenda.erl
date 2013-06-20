-module(runes_agenda).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-export([start/0,
	 get_conflict_set/1,
	 get_class_set/1,
	 get_process_set/1,
	 silent/1,
	 create_node/3,
	 put_pn/2,
	 get_pn/1,
	 delete_rn/1,
	 get_root_and_set_class/2,
	 get_node_num/1,
	 inc_node_num/1,
	 dec_node_num/1,
	 show_working_memory/0,
	 get_all_processes/0,
	 fire_able/2]).

-compile(export_all).

-behaviour(gen_server).

-define(SERVER,agenda).


-record(agenda, {conflict_set,class_set,process_set}).

start() ->
    {ok,Agenda} = gen_server:start_link(?MODULE,[],[]),
    register(agenda,Agenda).

create_node(Where,Type,Paras) ->
    gen_server:call({agenda,Where},{new,Type,Paras}).

get_root_and_set_class(Where,Class) ->
    gen_server:call({agenda,Where},{root,Class}).

get_dummy_top_node(Where) ->
    gen_server:call({agenda,Where},dtn).

fire_able(Pn,_State) ->
    gen_server:cast({agenda,node()},{fa,Pn}).

get_conflict_set(Where) ->
    gen_server:call({agenda,Where},get_cs).

get_class_set(Where) ->
    gen_server:call({agenda,Where},get_cls).

get_process_set(Where) ->
    gen_server:call({agenda,Where},get_ps).

silent(Where) ->
    gen_server:call({agenda,Where},silent).


init([]) ->
    ets:new(agenda, [public,named_table,{read_concurrency,true}]),
    Root = runes_compile:build_root_node_only_once(),
    Dummy_top_node = runes_compile:build_dummy_top_node_only_once(),
    ets:insert(agenda,{dummy_top_node,Dummy_top_node}),
    ets:insert(agenda,{root_node,Root}),
    ets:insert(agenda,{bm_num,0}),
    ets:insert(agenda,{am_num,0}),
    ets:insert(agenda,{ctn_num,0}),
    ets:insert(agenda,{pn_num,0}),
    ets:insert(agenda,{jn_num,0}),
    {ok,#agenda{conflict_set=[],class_set=[],process_set=[]}}.

handle_cast({fa,Pn},State) ->
    Cset0 = State#agenda.conflict_set,
    Cset1 = [Pn|Cset0],
    {noreply,State#agenda{conflict_set=Cset1}}.

handle_call({new,Type,Paras},_From,State) ->
    inc_node_num(Type),
    {ok,Pro} = runes_engine:create(Type,Paras),
    Pset0 = State#agenda.process_set,
    {reply,{ok,Pro},State#agenda{process_set=[Pro|Pset0]}};

handle_call({root,Class},_From,State) ->
    if Class /= nil ->
	    Old_Classes = State#agenda.class_set,
	    Where = runes_kb:find_class(Class),
	    if Where =:= no_class ->
		    runes_kb:insert(class,Class,node()),
		    {reply,
		     get_rn(),
		     State#agenda{class_set = [Class|Old_Classes]}};
	       true ->
		    {reply,get_rn(),State}
	    end;
       true ->
	    {reply,get_rn(),State}
    end;

handle_call(dtn,_From,State) ->
    {reply,get_dtn(),State};

handle_call(get_cs,_From,State) ->
    Set= State#agenda.conflict_set,
    %io:format("the length of conflict_set: ~p~n", [length(Set)]),
    {reply,{ok,Set},State};

handle_call(get_ps,_From,State) ->
    {reply,{ok,State#agenda.process_set},State};

handle_call(get_cls,_From,State) ->
    {reply,{ok,State#agenda.class_set},State};

handle_call(silent,_From,State) ->
    Pl = State#agenda.process_set,
   % io:format("~p ps: ~p~n",[self(),Pl]),
    {reply,isnull(Pl),State}.
    
   
handle_info(_Msg,State) ->
    {noreply,State}.

terminate(_Reason,_State) ->
    ok.

code_change(_OldVsn,State,_Extra) ->
    {ok,State}.

    
get_rn() ->
    case ets:lookup(agenda,root_node) of
	[{root_node,Root}] ->
	    {ok,Root};
	[] ->
	    no_root
    end.

get_dtn() ->
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
    io:format("All token_refs: ~p~n",[runes_kb:get_all_token_refs()]).

get_all_processes() ->
    [_|T] = nodes(),
    Nodes = [node()|T],
    lists:flatmap(fun(N) ->
			  {ok,Ps} = get_process_set(N),
			  Ps
		  end,Nodes).


loop(Pl) ->
    T2 = erlang:now(),
    Null = silent(Pl),
    io:format("~p",[z]),
    if Null ->
	    T3 = erlang:now(),
	    {T2,T3};
       true ->
	    loop(Pl)
    end.

isnull([]) ->
    true;
isnull([P|Pl]) ->
    Len = proplists:get_value(message_queue_len,process_info(P),1),
   % io:format("Len: ~p",[Len]),
    if Len /= 0 ->
	    false;
       true ->
	    isnull(Pl)
    end.

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
    
