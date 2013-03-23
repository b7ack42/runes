-module(runes_sup.erl).

-behaviour(supervisor).

-export([start_link/0, start_child/2]).

-compile(export_all).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({global, ?SERVER}, ?MODULE, []).

start_child([Type,Paras ]) ->
    supervisor:start_child({{global, ?SERVER},[Type, Paras]}. 

init([]) ->
    Rete_node = {runes_engine, {runes_engine, start_link, []},
		 temporary, brutal_kill, worker, [runes_engine]},
    Children = [Rete_node],
    RestartStrategy = [simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

