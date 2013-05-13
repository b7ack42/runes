-module(runes_p).

-include("internal.hrl").

-compile(export_all).

-export([]).



pmap(F,L) ->
    Parent = self(),
    Pids = lists:map(fun(I) ->
			     spawn(fun() -> Parent!{self(),{pm,F(I)}} end)
		     end,L),
    pmap_results(Pids).

pmap_results([H|T]) ->
    receive
	{H,{pm,Ret}} ->
	    [Ret|pmap_results(T)]
    end;
pmap_results([]) ->
    [].

pflatmap(F,L) ->
    Parent = self(),
    Pids = lists:map(fun(I) ->
			     spawn(fun() -> Parent!{self(),{pfm,F(I)}} end)
		     end,L),
    pfmap_results(Pids).

pfmap_results([H|T]) ->
    receive
	{H,{pfm,Ret}} ->
	    Ret ++ pfmap_results(T)
    end;
pfmap_results([]) ->
    [].



pforeach(F,L) ->
    lists:foreach(fun(X) ->
		      spawn(fun()->
				    F(X)
			    end)
	      end,L).

pfilter(F,L) ->
    Parent = self(),
    Pids = lists:map(fun(I) ->
			     spawn(fun() -> Parent!{self(),{pf,F(I),I}} end)
		     end,L),
    pf_results(Pids).

pf_results([H|T]) ->
    receive
	{H,{pf,Bool,Ret}} ->
	    if Bool ->
		    [Ret|pf_results(T)];
	       true ->
		    pf_results(T)
	    end
    end;
pf_results([]) ->
    [].

pmap2(F,L) ->
    Parent = self(),
    pmap2_results(
      lists:foldl(fun(X,I) ->
			  spawn_link(fun()->
					     Parent!{pmap,{I,F(X)}}
				     end),I+1
		  end,0,L),[]).

pmap2_results(0,Rs) ->
    lists:map(fun({_,R}) ->
		      R
	      end,lists:keysort(1,Rs));
pmap2_results(N,Rs) ->
    pmap2_results(N-1,receive{pmap,R} ->
			     [R|Rs]
		     end).
