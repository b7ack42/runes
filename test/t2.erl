-module(t2).

-compile(export_all).

-record(rule, {name, lhs, rhs}).

-record (fields, {id,
		  attr,
		  value}).

-record (wme, {fields,
	       alpha_mems,    %list
	       token_refs}).

-record (token, {parent,
		 wme_ref,    %list
		 node,
		 children,
		 join_results,
		 owner}).

init() ->
    application:start(runes).
   % appmon:start().

stop() ->
    application:stop(runes).
    %appmon:stop().

start(N) ->
    {Time,_} = timer:tc(t1,makerb,[N]),
    Time/1000000.

makerb(N) ->
    Rules = makerules(N),
    lists:foreach(fun(R)->
			  %io:format("rule: ~p",[R]),
			  runes:add_rule(R)
		  end,Rules),    
    ok.
    
ex(N,M) ->
    stop(),
    init(),
    Pl1 = processes(),
    Ti =  start(N),
    io:format("time to make rete net of ~p rules: ~p~n",[N,Ti]),
    Pl2 = processes(),
    io:format("Pl1: ~p   Pl2: ~p~n",[length(Pl1),length(Pl2)]),
    Pl = delete_list(Pl1,Pl2),
    T1 = erlang:now(),
%    addfacts(M),
    {T2,T3} = loop(Pl),
    %lists:foreach(fun(P) ->
%			  io:format("the state of ~p: ~p~n",[P,P!state])
%		  end,Pl),
    Diff1 = timer:now_diff(T2,T1)/1000000,
    Diff2 = timer:now_diff(T3,T1)/1000000,
    io:format("Diff1: ~p,  Diff2:~p~n",[Diff1,Diff2]),
    Pl.
    
    

loop(Pl) ->
    T2 = erlang:now(),
    Null = isnull(Pl),
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

make_value(V) -> 
    case V of
	1 ->
	    {var,x};
	2 ->
	    {var,y};
	3 ->
	    {var,z};
	_ ->          %4,5,...
	    %io:format("h~n"),
	    {con,V}
    end.

make_cond(Id,N,U) ->
    Attr = case N of
	       1 -> u;
	       2 -> v;
	       3 -> w
	   end,
    #fields{id={con,Id},attr={con,Attr},value=make_value(random:uniform(U))}.

makerule(Nth) ->   
    U = 7,
    Ni = 4,
    Na = 3,
    Conds = lists:flatten(lists:map(fun(X)->
					  lists:map(fun(Y) ->
							    make_cond(X,Y,U) 
						    end,lists:seq(1,Na))
				  end,lists:seq(1,Ni))),
    io:format("Conds: ~p, Conds~n",[Conds]),
    #rule{name=Nth,lhs=Conds,rhs=ok}.
   


makerules(Num) ->
    random:seed(erlang:now()),
    lists:map(fun(Nth) ->
		      makerule(Nth) 
	      end,lists:seq(1,Num)).

addfacts(0) ->
    ok;
addfacts(Num) ->
    random:seed(erlang:now()),
    lists:foreach(fun(_N) ->
			  addfact()
		  end,lists:seq(1,Num)).

addfact() ->
    Ni = 4,
    Na = 3,
    U = 7-3,
    Id =random:uniform(Ni),
    Attr =n2c(random:uniform(Na)),
    Value = random:uniform(U),
    Fact = #fields{id=Id,attr=Attr,value=Value},
    runes:add_fact(Fact).
    
n2c(1) ->
    u;
n2c(2) ->
    v;
n2c(3) ->
    w.

delete_list(L1,L2) ->  
    lists:foldl(fun(E,L) ->
			lists:delete(E,L)
		end,L2,L1).

makefact(I,A,V) ->
    #fields{id=I,attr=A,value=V}.




