-module(runes_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/2]).

-export([init/1]).

-compile(export_all).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Type,Paras ) ->
    supervisor:start_child(?SERVER,[Type, Paras]). 

init([]) ->
    Rete_node = {runes_engine, {runes_engine, start_link, []},
		 transient, 1000, worker, [runes_engine]},
    Children = [Rete_node],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

