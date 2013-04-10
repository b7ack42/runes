-module(test).

-export([start/0,init/0]).

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
    application:start(runes),
    appmon:start().

stop() ->
    application:stop(runes),
    appmon:stop().

start() ->
stop(),
init(),
    C1 =#fields{id={con,1},attr={var,x},value={var,x}},
    C2 =#fields{id={con,1},attr={con,color},value={var,x}},
    C3 =#fields{id={con,2},attr={con,color},value={con,yellow}},
    C4 =#fields{id={con,2},attr={con,class},value={con,tank}},
    C5 =#fields{id={con,3},attr={var,x},value={con,car}},
    C6 =#fields{id={con,3},attr={var,x},value={con,red}},
    C7 =#fields{id={con,3},attr={var,y},value={var,x}},
    C8=#fields{id={con,4},attr={con,class},value={con,tank}},
    C9=#fields{id={var,y},attr={con,class},value={con,red}},
    Conds1 = [C1,C2],
    Conds2 = [C3,C4],
    Conds3 = [C5,C6],
    Conds4= [C5,C6,C7,C8,C9],
    Rule1= #rule{name=1,lhs=Conds1,rhs={ok}},
    Rule2 = #rule{name=2,lhs=Conds2,rhs={ok}},
    Rule3= #rule{name=3,lhs=Conds3,rhs={ok}}, 
    Rule4 = #rule{name=4,lhs=Conds4,rhs={ok}},
    %runes:add_rule(Rule3),
    runes:add_rule(Rule1).

rl(N) ->
	[{N,Pn}] = ets:lookup(agenda,N),
	runes_engine:delete(Pn).

rw() ->
	[Wr1,Wr2] = runes_kb:get_all_wme_refs(),
	%runes:info(),
        runes_engine:remove_wme(Wr1),
	%runes:info(),
        runes_engine:remove_wme(Wr2).
	%runes:info().

add_fact1() ->
    F1 = #fields{id={con,3},attr={con,color},value={con,red}},
    F2 = #fields{id={con,3},attr={con,color},value={con,car}},
    W1 = #wme{fields=F1,alpha_mems=[],token_refs=[]},
    W2 = #wme{fields=F2,alpha_mems=[],token_refs=[]},
    runes:add_fact(W1),
    runes:add_fact(W2).

add_fact2() ->
    F1 = #fields{id={con,3},attr={con,test},value={con,car}},
    F2 = #fields{id={con,3},attr={con,test},value={con,red}},
    F3 =#fields{id={con,3},attr={con,color},value={con,test}},
    W1 = #wme{fields=F1,alpha_mems=[],token_refs=[]},
    W2 = #wme{fields=F2,alpha_mems=[],token_refs=[]},
    W3 = #wme{fields=F3,alpha_mems=[],token_refs=[]},
    runes:add_fact(W1),
    runes:add_fact(W2),
    runes:add_fact(W3).
