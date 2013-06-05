-module(runes).

-export([
	 add_rule/1,
	 remove_rule/1,
	 add_fact/1,
	 fire/0,
	 info/0
	]).

-include("internal.hrl").

-compile(export_all).

add_rule(Rule) ->
    runes_compile:add_production(Rule).

remove_rule(Rule) ->
    runes_compile:remove_production(Rule).

add_fact(Fact) ->
    Wme = #wme{fields=Fact,alpha_mems=[],token_refs=[]},
    runes_engine:add_wme(Wme).

info() ->
    runes_agenda:show_working_memory(),
    io:format("All classes: ~p~n",[runes_agenda:get_class_set(node())]),
    runes_agenda:get_node_num(all_nodes).

fire() ->
    [_|T] = nodes(),
    Nodes = [node()|T],
    lists:map(fun(N) ->
		      {ok,CSet} = runes_agenda:get_conflict_set(N),
		      io:format("the length of conflict_set at ~p: ~p~n",[N,length(CSet)]),
		      {N,CSet}
	      end,Nodes).
    


    
