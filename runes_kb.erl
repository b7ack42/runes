-module(runes_kb).
-export([init/0,
	 make_wm_ref/1,
	 make_token_ref/1,
	 make_token/3,
	 insert/3,
	 delete/2,
	 get_am/1,
	 get_bm/1,
	 get_wme/1,
	 find_class/1,
	 get_f/2,
	 get_token/1,
	 get_nth_wr_from_token/2,
	 get_all_wme_refs/0,
	 find_all_classes/0
	]).

-compile(export_all).

-include("internal.hrl").

%5-behavior(gen_server).

-define(WAIT_FOR_TABLES,3000).

-record(class_store,{class,node}).
-record(wm_store,{wr,wme}).
-record(token_store,{tr,token}).
-record(bm_store,{pid,bm}).
-record(am_store,{pid,am}).

init() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),
    {ok, CacheNodes} = resource_discovery:fetch_resources(runes),
    dynamic_db_init(lists:delete(node(),CacheNodes)),
   % ets:new(wm_store, [public, named_table, {read_concurrency, true}]),
   % ets:new(token_store, [public, named_table,{read_concurrency, true}]),
   % ets:new(bm,[public,named_table,{read_concurrency,true}]),     
   % ets:new(am,[public,named_table,{read_concurrency,true}]),
    %5{ok,Kb} = gen_server:start_link(?MODULE,[],[]),
    %5register(kb,Kb),
    ok.

make_wm_ref(Wme) ->
%    Ref = make_ref(),
    Ref = ref:new_ref(),
    insert(wm,Ref,Wme),
    Ref.

make_token_ref(Token) ->
    %Ref = make_ref(),
    Ref = ref:new_ref(),
    insert(token,Ref,Token),
    Ref.

make_token(Node,Tr,Wr) ->
    Token =  #token{parent = Tr,
		    wme_ref = Wr,
		    node = Node,
		    children = []},
    Ref = make_token_ref(Token),
    Wme0 = get_wme(Wr),
    Wme_trs0 = Wme0#wme.token_refs,
    Wme1 = Wme0#wme{token_refs = [Ref|Wme_trs0]},
    insert(wm,Wr,Wme1),
    if Tr /= nil ->
	    Parent = get_token(Tr),
	    PChildren = Parent#token.children,
	    Parent1 = Parent#token{children = [Ref|PChildren]},
	    insert(token,Tr,Parent1);
       true ->
	    ok
    end,
    Ref.
  
insert(class,Class,Where) ->
   %5 mnesia:dirty_write(#class_store{class=Class,node=Where});
    mnesia:transaction(
      fun()->
	      mnesia:write(#class_store{class=Class,node=Where})
      end);
insert(wm, Ref, Wme) ->
   %5 mnesia:dirty_write(#wm_store{wr=Ref,wme=Wme});
%    ets:insert(wm_store,{Ref, Wme});
    mnesia:transaction(
      fun() ->
	      mnesia:write(#wm_store{wr=Ref,wme=Wme})
      end);    
insert(token, Ref, Token) ->
%5    mnesia:dirty_write(#token_store{tr=Ref,token=Token}).
 %   ets:insert(token_store, {Ref, Token}).
    mnesia:transaction(
      fun() ->
	      mnesia:write(#token_store{tr=Ref,token=Token})
      end).

delete(am,Am) ->
    mnesia:dirty_delete({am_store,Am});
    %ets:delete(am,Am);
delete(bm,Bm) ->
    mnesia:dirty_delete({bm_store,Bm});
%    ets:delete(bm,Bm);
delete(wm,Wme_ref) ->
    mnesia:dirty_delete({wm_store,Wme_ref});
%   ets:delete(wm_store,Wme_ref);
delete(token,Tr) ->
    mnesia:dirty_delete({token_store,Tr}).
    %ets:delete(token_store,Tr).

set_am(Am,Mem) ->
    mnesia:dirty_write(#am_store{pid=Am,am=Mem}).
    %ets:insert(am,{Am,Mem}).

set_bm(Bm,Mem) ->
    %ets:insert(bm,{Bm,Mem}).
    mnesia:dirty_write(#bm_store{pid=Bm,bm=Mem}).

get_am(Am) ->
    %case ets:lookup(am,Am) of
    case mnesia:dirty_read(am_store,Am) of
	[{am_store,Am,Mem}] ->
	    Mem;
	[] -> []
    end.
get_bm(Bm) ->
    %case ets:lookup(bm,Bm) of
    case mnesia:dirty_read(bm_store,Bm) of
	[{bm_store,Bm,Mem}] ->
	    Mem;
	[] ->
	    []
    end.

find_class(Class) ->
    case mnesia:dirty_read(class_store,Class) of
	[{class_store,Class,Where}] ->
	    Where;
	[] ->
	    no_class
    end.

get_wme(Ref) ->
%   case ets:lookup(wm_store,Ref) of
    case mnesia:dirty_read(wm_store,Ref) of
       [{wm_store,Ref,Wme}] ->
	   Wme;
       [] ->
%5	    {atomic,[{wm_store,Ref,Wme}]} = 
%5		mnesia:transaction(
%5		  fun() ->
%5			  mnesia:read(wm_store,Ref)
%5		  end),
%	    Wrs = get_all_wme_refs(),
%	    Bool = lists:member(Ref,Wrs),
%	    io:format("NO WME FOUND: ~p Bool: ~p~n",[Ref,Bool]),
	   no_wme
%5	    Wme.
   end.

get_f(Wme, Field) ->
    try 
	Fields = Wme#wme.fields,
	proplists:get_value(Field,Fields)
    catch
	error:_Error -> io:format("Wme:~p~n",[Wme]),
			error
    end.
%    case Field of
%	id ->  Fields#fields.id;
%	attr -> Fields#fields.attr;
%	value -> Fields#fields.value
%    end.

get_token(Ref) ->
    if Ref == nil ->
	    [];
       true ->
	    %case ets:lookup(token_store,Ref) of
	    case mnesia:dirty_read(token_store,Ref) of
		[{token_store,Ref,Token}] ->
		    Token;
		[] -> no_Token
	    end
    end.

get_nth_wr_from_token(Token_ref,0) ->
    Token = get_token(Token_ref),
    Token#token.wme_ref;    
get_nth_wr_from_token(nil,_) ->
    nil;
get_nth_wr_from_token(Token_ref,Nth) ->
    Nth1 = Nth-1,
    Token = get_token(Token_ref),
    Parent_r = Token#token.parent,
    get_nth_wr_from_token(Parent_r,Nth1).

get_all_wme_refs() ->
    {atomic,Wrs} = 
	mnesia:transaction(
	  fun() ->
		  mnesia:select(wm_store,[{#wm_store{wr='$1',wme='_'},[],['$1']}])
	  end),
    Wrs.

get_all_token_refs() ->
    {atomic,Trs} = 
	mnesia:transaction(
	  fun() ->
		  mnesia:select(token_store,[{#token_store{tr='$1',token='_'},[],['$1']}])
	  end),
    Trs.

find_all_classes() ->    
    {atomic,Class_Nodes} = 
	mnesia:transaction(
	  fun() ->
		  mnesia:select(class_store,[{#class_store{class='$1',node='$2'},
					      [],['$$']}])
	  end),
    Class_Nodes.

dynamic_db_init([]) ->
    {atomic,ok} = 
	mnesia:create_table(class_store,
			    [{attributes,record_info(fields,class_store)}]),
    {atomic,ok} =
        mnesia:create_table(wm_store,
			    [{attributes,record_info(fields,wm_store)}]),
    {atomic,ok} =
	mnesia:create_table(token_store,
			[{attributes,record_info(fields,token_store)}]),
    {atomic,ok} = 
	mnesia:create_table(bm_store,
			[{attributes,record_info(fields,bm_store)}]),
    {atomic,ok} =
	mnesia:create_table(am_store,
			    [{attributes,record_info(fields,am_store)}]);
dynamic_db_init(Nodes) ->
    add_extra_nodes(Nodes) .

add_extra_nodes([Node|T]) ->
    case mnesia:change_config(extra_db_nodes,[Node]) of
	{ok,[Node]} ->
	    mnesia:add_table_copy(schema,node(),ram_copies),
	    mnesia:add_table_copy(class_store,node(),ram_copies),
	    mnesia:add_table_copy(wm_store,node(),ram_copies),
	    mnesia:add_table_copy(token_store,node(),ram_copies),
	    mnesia:add_table_copy(am_store,node(),ram_copies),
	    mnesia:add_table_copy(bm_store,node(),ram_copies),
	    Tables = mnesia:system_info(tables),
	    mnesia:wait_for_tables(Tables,?WAIT_FOR_TABLES);
	_ ->
	    add_extra_nodes(T)
    end.
    
	    
	
    






