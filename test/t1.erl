-module(t1).

-compile(export_all).

-record(rule, {name, lhs, rhs}).

%-record(lhs, {name,conds}).

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
    %application:start(mnesia),
    random:seed(erlang:now()),
    application:start(resource_discovery),
    application:start(runes).
   % appmon:start().

stop() ->
    application:stop(mnesia),
    application:stop(runes).
    %appmon:stop().

start(N) ->
    Rules = makerules(N),
    {Time,_} = timer:tc(t1,makerb,[Rules]),
    Time/1000000.

makerb(Rules) ->
    %Rules = makerules(N),
    lists:foreach(fun(R)->
			  %io:format("rule: ~p",[R]),
			  runes:add_rule(R)
		  end,Rules),    
    ok.
 

ar() ->
    C1 =#fields{id={con,1},attr={con,u},value={var,x}},
    C2 =#fields{id={con,1},attr={con,v},value={con,6}},
    C3 =#fields{id={con,1},attr={con,w},value={con,5}},
    C4 =#fields{id={con,2},attr={con,u},value={con,5}},
    C5 =#fields{id={con,2},attr={con,v},value={con,4}},
    C6 =#fields{id={con,2},attr={con,w},value={con,6}},
    C7 =#fields{id={con,3},attr={con,u},value={var,x}},
    C8 =#fields{id={con,3},attr={con,v},value={con,6}},
    C9 =#fields{id={con,3},attr={con,w},value={con,4}},
    C10=#fields{id={con,4},attr={con,u},value={var,x}},
    C11=#fields{id={con,4},attr={con,v},value={con,7}},
    C12=#fields{id={con,4},attr={con,w},value={var,z}},

    Conds = [C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12],
    Rule = #rule{name=1,lhs=Conds,rhs={ok}},
    Pl1 = processes(),
    runes:add_rule(Rule),
    Pl2 = processes(),
    Pl2--Pl1.
   
ex(N,M) ->
    stop(),
    init(),
   % Pl1 = processes(),
    Ti =  start(N),
    io:format("time to make rete net of ~p rules: ~p~n",[N,Ti]),
   % Pl2 = processes(),
    %io:format("Pl1: ~p   Pl2: ~p~n",[length(Pl1),length(Pl2)]),
    %Pl = Pl2--Pl1,
    %Pl = ar(),
    T1 = erlang:now(),
    addfacts(M),
    %addfacts2(M,Pl),
    {T2,T3} = loop(),
    %lists:foreach(fun(P) ->
%			  io:format("the state of ~p: ~p~n",[P,P!state])
%		  end,Pl),
    Diff1 = timer:now_diff(T2,T1)/1000000,
    Diff2 = timer:now_diff(T3,T1)/1000000,
    io:format("~nDiff1: ~p,  Diff2:~p~n",[Diff1,Diff2]),
    ok.
    %Pl.
     
ex2(N,M) ->
    Ti =  start(N),
    io:format("time to make rete net of ~p rules: ~p~n",[N,Ti]),
    T1 = erlang:now(),
    addfacts(M),
    %addfacts2(M,Pl),
    {T2,T3} = loop(),
    Diff1 = timer:now_diff(T2,T1)/1000000,
    Diff2 = timer:now_diff(T3,T1)/1000000,
    io:format("~nDiff1: ~p,  Diff2:~p~n",[Diff1,Diff2]),
    ok.

show(Pl) ->
    timer:sleep(3000),
    runes:info(),
    mem(Pl),
    io:format("~n"),
    timer:sleep(100),
    test(Pl),
    io:format("~n"),
    timer:sleep(100),
    io:format("~n"),
    runes:fire().
   
sh() -> 
    %timer:sleep(3000),
   %runes:info(),
    loop(),
    runes:fire(),
   % Pl.
    ok.

log(L) ->
    lists:foreach(fun(N) ->
			  io:format("~p~n",[N])
		  end,L).

mem(Pl) ->
    lists:foreach(fun(P) ->
			  P!mem
		  end,Pl).

meml(Pl) ->
    lists:foreach(fun(P) ->
			  P!ml
		  end,Pl).

msg(Pl) ->
    lists:foreach(fun(P) ->
			  {P,proplists:get_value(message_queue_len,process_info(P),nil)}
		  end,Pl).
msgl(Pl) ->
    lists:map(fun(P) ->
		      {P,proplists:get_value(message_queue_len,process_info(P),nil)}
		  end,Pl).
test(Pl) ->
    lists:foreach(fun(P) ->
			  P!test
		  end,Pl).

state(Pl) ->
    lists:foreach(fun(P) ->
			  P!state
		  end,Pl).

rw(Wrs) ->
    lists:foreach(fun(Wr) ->
			  runes_engine:remove_wme(Wr)
		  end,Wrs).   

loop(Pl) ->
    T2 = erlang:now(),
    Null = isnull(Pl),
    io:format("~p",[z]),
    if Null ->
	    T3 = erlang:now(),
	    {T2,T3};
       true ->
	    loop(Pl)
    end.


loop() ->
    T2 = erlang:now(),
   % Null = isnull(Pl),
    [_|T] = nodes(),
    Nodes = [node()|T],
    Free = free(Nodes),
    io:format("~p",[z]),
    if Free ->
	    T3 = erlang:now(),
	    {T2,T3};
       true ->
	    loop()
    end.

free([]) ->
    true;
free([N|Ns]) ->
    Null = runes_agenda:silent(N),
    if Null ->
	    free(Ns);
       true ->
	    flase
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

ploop(Pl) ->
    T2 = erlang:now(),
    io:format("~p",[p]),
    Pr = 
	runes_p:pmap(fun(P) ->
			     Len = proplists:get_value(message_queue_len,process_info(P),1),
			     if Len /= 0 ->
				     false;
				true ->
				     true
			     end
		     end,Pl),
    Null = lists:foldl(fun(E,Acc) -> 
			       Acc andalso E
		       end,true,Pr),
    if Null ->
	    T3 = erlang:now(),
	    {T2,T3};
       true ->
	    ploop(Pl)
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

make_cond(Al) ->
    Classes = ['A','B','C','D','E','F','G',
	       'H','I','J','K','L','M','N',
	       'O','P','Q','R','S','T',
	       'U','V','W','X','Y','Z'],
    Cc = lists:nth(random:uniform(26),Classes),
    %    #fields{id={con,Id},attr={con,Attr},value=make_value(random:uniform(U))}.
    Re = lists:map(fun({U,A}) ->
			   if A > 5 ->
				   {U, {var,U}};
			      true ->
				   {U,{con,A}}
			   end
		   end,lists:zip([u,v,w],Al)),
    [{class,{con,Cc}}|Re].
    
    

makerule(Nth) ->   
    U = 10,
    Nc =4,
    Na =3,
    Mat = 
	lists:map(fun(_A) ->
			  {Ar,Ct,Pos} = lists:foldr(fun(C,{Rs,Count,P}) ->
							    R = random:uniform(U),
							    if R > 5 ->
								    {[R|Rs],Count+1,C-1};
							       true ->
								    {[R|Rs],Count,P}
							    end
						    end,{[],0,-1},lists:seq(1,Nc)),
			  if Ct == 1 ->
				  {L1,[_N|L2]} =  lists:split(Pos,Ar),
				  L1 ++ [random:uniform(5)|L2];
			     true ->
				  Ar
			  end
		  end,lists:seq(1,Na)),
    Conds = lists:map(fun(N) ->
			      make_cond(lists:map(fun(N2) ->
						     lists:nth(N,lists:nth(N2,Mat))
						  end,lists:seq(1,Na)))
		      end,lists:seq(1,Nc)),
    %io:format("~n~p~n",[Conds]),
    #rule{name=Nth,lhs=Conds,rhs=ok}.
   
makerules(Num) ->
    random:seed(erlang:now()),
    lists:map(fun(Nth) ->
		      makerule(Nth) 
	      end,lists:seq(1,Num)).

addfacts2(0,_Pl) ->
    ok;
addfacts2(Num,Pl) ->
    random:seed(erlang:now()),
    lists:foreach(fun(N) ->
			  %loop(Pl),
			  io:format("~nleft: ~p~n",[Num-N]),
			  loop(Pl),
			  addfact()
		  end,lists:seq(1,Num)).

addfacts(0) ->
    ok;
addfacts(Num) ->
    random:seed(erlang:now()),
    lists:foreach(fun(_N) ->
			  %timer:sleep(100),
			  addfact()
		  end,lists:seq(1,Num)).

addfact() ->
    Classes = ['A','B','C','D','E','F','G',
		'H','I','J','K','L','M','N',
		'O','P','Q','R','S','T',
		'U','V','W','X','Y','Z'],
    Ni = 4,
    Na = 3,
    Uni = 5, %10/2
    C = lists:nth(random:uniform(26),Classes),
    U =random:uniform(Uni),
    V =random:uniform(Uni),
    W =random:uniform(Uni),
    Fact = makefact(C,U,V,W),
   % io:format("fact: ~p~n",[Fact]),
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

makefact(C,U,V,W) ->
    [{class,C},{u,U},{v,V},{w,W}].
