-module(runes_kb).
-export([]).

-compile(export_all).

-include("internal.hrl").

init() ->
    ets:new(wm_store, [public, named_table, {read_concurrency, true}]),
    ets:new(token_store, [public, named_table,{read_concurrency, true}]),  
    ok.

make_wm_ref(Wme) ->
    Ref = make_ref(),
    insert(wm,Ref,Wme),
    Ref.

make_token_ref(Token) ->
    Ref = make_ref(),
    insert(token,Ref,Token),
    Ref.

make_token(Node,Tr,Wr) ->
    Token =  #token{parent = Tr,
		    wme_ref = Wr,
		    node = Node,
		    children = []},
    Ref = make_token_ref(Token),
    Parent = get_token(Tr),
    PChildren = Parent#token.children,
    Parent1 = Parent#token{children = [Ref|PChildren]},
    insert(token,Tr,Parent1),
    Ref.
    

insert(wm, Ref, Wme) ->
    ets:insert(wm_store,{Ref, Wme});
insert(token, Ref, Token) ->
    ets:insert(token_store, {Ref, Token}).

delete(wm,Wme_ref) ->
    ets:delete(wm_store,Wme_ref);
delete(token,Tr) ->
    ets:delete(token_store,Tr).

get_wme(Ref) ->
    case ets:lookup(wm_store,Ref) of
	[{Ref,Wme}] ->
	    Wme;
	[] -> no_wme
    end.

get_f(Wme, Field) ->
    Fields = Wme#wme.fields,
    case Field of
	id ->  Fields#fields.id;
	attr -> Fields#fields.attr;
	value -> Fields#fields.value
    end.

get_token(Ref) ->
    case ets:lookup(token_store,Ref) of
	[{Ref,Token}] ->
	    Token;
	[] -> no_Token
    end.

get_nth_wr_from_token(Token_ref,0) ->
    Token_ref#token.wme_ref;    
get_nth_wr_from_token(Token_ref,Nth) ->
    Nth1 = Nth-1,
    Token = get_token(Token_ref),
    Parent_r = Token#token.parent,
    get_nth_wr_from_token(Parent_r,Nth1).

get_all_wme_refs() ->
    Pairs = ets:match(wm_store,{'$1','_'}),
    lists:flatten(Pairs).
    


    
	    
	
    






