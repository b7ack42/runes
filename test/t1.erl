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
			  runes:add_rule(R)
		  end,Rules),    
    ok.
    
   

make_value(V) -> 
    case V of
	1 ->
	    {var,x};
	2 ->
	    {var,y};
	3 ->
	    {var,z};
	_ ->          %1,2,3,4
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
    random:seed(erlang:now()),
    Conds = lists:flatten(lists:map(fun(X)->
					  lists:map(fun(Y) ->
							    make_cond(X,Y,U) 
						    end,lists:seq(1,Na))
				  end,lists:seq(1,Ni))),
    #rule{name=Nth,lhs=Conds,rhs=ok}.

makerules(Num) ->
    lists:map(fun(Nth) ->
		      makerule(Nth) 
	      end,lists:seq(1,Num)).
    
     
    
    
