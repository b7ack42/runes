-module(ref).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-export([new_ref/0
	 ]).

-compile(export_all).

-behaviour(gen_server).

-define(SERVER,?MODULE).

-record(state,{ref_num}).

start() ->
    gen_server:start_link({global,?SERVER},?MODULE,[],[]).

new_ref() ->
    gen_server:call({global,?SERVER},mr).

init([]) ->
    {ok,#state{ref_num=0}}.

handle_cast(_Msg, State) ->
    {noreply,State}.    

handle_call(mr,_From,State) ->
    Ref_num = State#state.ref_num,
    {reply,make_ref(),State#state{ref_num = Ref_num+1}}.

handle_info(_Msg,State) ->
    {noreply,State}.

terminate(_Reason,_State) ->
    ok.

code_change(_OldVsn,State,_Extra) ->
    {ok,State}.
