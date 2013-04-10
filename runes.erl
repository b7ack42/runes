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
    runes_engine:add_wme(Fact).

info() ->
    runes_agenda:show_working_memory(),
    runes_agenda:get_node_num(all_nodes).

fire() ->
    runes_agenda:get_conflict_set().

    


    
