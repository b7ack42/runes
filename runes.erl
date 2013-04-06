-module(runes).

-export([
	 add_rule/1,
	 remove_rule/1,
	 add_fact/1,
	 fire/0	 
	]).

-include("internal.hrl").

-compile(export_all).

add_rule(Rule) ->
    runes_compile:add_production(Rule).

remove_rule(Rule) ->
    runes_compile:remove_production(Rule).

add_fact(Fact) ->
    runes_engine:add_wme(Fact).

fire() ->
    ok.
    


    
