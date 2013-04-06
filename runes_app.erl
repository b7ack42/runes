-module(runes_app).

-behaviour(application).

-export([start/2,stop/1]).

start(_StartType, _StartArgs) ->
    runes_kb:init(),
    case runes_sup:start_link() of
	{ok,Pid} ->
	    runes_agenda:init(),
	    {ok, Pid};
	Other->
	    {error,Other}
    end.

stop(_State) ->
    ok.
