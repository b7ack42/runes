-module(runes_engine).

-behaviour(gen_server).

-export([]).

-export([]).

-include("internal.hrl").

-compile(export_all).

-ifdef(debug).
-define(LOG(F,X), io:format(F,X)).
-else.
-define(LOG(F,X), ok).
-endif.

-define(SERVER, ?MODULE).


create(Type, Paras) ->
    runes_sup:start_child(Type,Paras).

start_link(alpha_memory, Paras) ->
    gen_server:start_link(?MODULE, [alpha_memory, Paras], []);
start_link(constant_test_node, Paras) ->
    gen_server:start_link(?MODULE, [constant_test_node,Paras],[]);
start_link(beta_memory,Paras) ->
    gen_server:start_link(?MODULE, [beta_memory,Paras], []);
start_link(join,Paras) ->
    gen_server:start_link(?MODULE, [join_node, Paras], []);
start_link() ->


add_wme(Wme) ->
    Wme_ref = runes_kb:make_wm_ref(Wme),
    constant_test_node_activation(root_node, Wme_ref).

constant_test_node_activation(CNode,Wme_ref) ->
    gen_server:cast(CNode,{ca, Wme_ref}).

alpha_memory_activation(Node, Wme_ref) ->
    gen_server:cast(Node, {aa,Wme_ref}).

beta_memory_left_activation(Bnode,Token_ref,Wme_ref) ->
    gen_server:cast(Bnode,{bla,Token_ref,Wme_ref}).

join_node_left_activation(Jnode,Token_ref,Flag) ->
    gen_server:cast(Jnode,{jla,Token_ref,Flag}).

join_node_right_activation(Jnode,Wr,Flag) ->
    gen_server:cast(Jnode,{jra,Wr,Flag}).

left_activation({Node,Type},Tr,Tag) ->
    case Type of
	{rete_node,join} ->
	    join_node_left_activation(Node,Tr,Tag);
	{rete_node,bm} -> 
	    beta_memory_left_activation(Node,Tr);
	_ -> 
	    LOG("no such node as ~p : ~p",[Type,self()])
    end.

right_activation({Node,Type},Wr,Tag) ->
    case Type of
	{rete_node,join} ->
	    join_node_right_activation(Node,Wr,Flag);
	_ ->
	    LOG("no such node as ~p : ~p",[Type,self()])
    end.



remove_wme(Wme_ref)->
    Wme = runes_kb:get_wme(Wme_ref),
    #wme{alpha_mems = Amems,token_refs = Trs} = Wme,% Amems = Wme#wme.alpha_mems,
    lists:foreach(fun(Amem) ->
			  remove(wme,Wme_ref,Amem)
		  end,Amems),
    lists:foreach(fun(Tr) ->
			  delete_token_and_descendents(Tr)
		  end,Trs),
    runes_kb:delete(wm,Wme_ref).

delete_token_and_descendents(Tr) ->
    #token{node = Node,wme_ref = Wr,children = Chn,parent = Pr}
	= runes_kb:get_token(Tr),
    lists:foreach(fun(Child) ->
			  delete_token_and_descendents(Child)
		  end,Chn),
    if Wr /= nil ->
	    Wme = runes_kb:get_wme(Wr),
	    Trs = Wme#wme.token_refs,
	    Wme1 = Wme#wme{token_refs = lists:delete(Tr,Trs)},
	    runes_kb:insert(wm,Wr,Wme1)
    end,
    Pt0 = runes_kb:get_token(Pr),
    Pchn = lists:delete(Tr,Pt0#token.children),
    Pt1 = Pt0#token{children = Pchn},
    runes_kb:insert(Pr,Pt1),
    remove(token,Tr,Node),
    runes_kb:delete(token,Tr).

relink_to_alpha_memory(JNode,Amem)->
    gen_server:cast(Amem,{l2m,JNode}).
relink_to_beta_memory(JNode,Bmem) ->
    gen_server:cast(Bmem,{l2m,JNode}).
    
link_to(Node,Tnode) ->
    gen_server:cast(Tnode,{ln,Node}).

dislink_to(Node,FNode) ->
    gen_server:cast(FNode,{rn,Node}).
remove(wme,Wme_ref,Amem) ->
    gen_server:cast(Amem,{rw,Wme_ref});
remove(token,Token_ref,Node) ->
    gen_server:cast(Node,{rt,Token_ref}).

%%??
removed(Node) ->
    gen_server:cast(Node,rmd).
dislink2am(Node)->
    gen_server:cast(Node,d2a).




quarry_mem(Node) ->
    gen_server:call(Node,quarry_mem).
quarry_keys(Node,Keys) ->
    gen_server:call(Node,{quarry_keys,Keys}).
is_mem_nil(Node) ->		
    gen_server:call(Node,is_nil).

find_ancestor(JNode) ->
    gen_server:call(JNode,find_ancestor).
is_right_unlinked(Amem) ->
    {ok,Is_in} = gen_server:call(Amem,is_in_mem),
    {ok,not(Is_in)}.





handle_cast({am_activation,Wme_ref},State) ->
    #alpha_memory{wme_refs = Wme_refs0, succs = Succs, ref_count = Rc0} = State,
    Wme_refs1 = [Wme_ref|Wmes_ref0],
    if Wme_refs0 == [] -> Flag = true;
       true            -> Flag = false
    end,
    Rc1 = Rc0 + 1,  %%%???
    Wme0 = runes_kb:get_wme(Wme_ref),
    
    #wme{alpha_mems = Alpha_mems0} = Wme0,
    Wme1 = Wme0#wme{alpha_mems = [self()|Alpha_mems0]},
    runes_kb:insert(wm,Wme_ref,Wme1),
    lists:foreach(fun(Pair)->
 			  right_activation(Pair,Wme_ref,Flag)
 		  end,Succs),
    {noreply, State#alpha_memory{wmes=Wme_refs1, ref_count=Rc1}};

handle_cast({ctn_activation,Wme_ref}, State) ->
    #constant_test_node{field = Fi,
			value = Va,
			out_put_mem = Om,
			children = Chn} = State,
    if 	Fi /= no_test ->
	    Wme = runes_kb:get_wme(Wme_ref),	    
 	    V2 = runes_kb:get_f(Wme,Fi),
 	    if V2 /= Va ->
 		    Flag = 0;
 	       true  ->
 		    Flag = 1
 	    end;
 	true -> Flag = 1
    end,
    if Flag == 1 ->
	    if Om /= nil -> alpha_memory_activation(Om, Wme_ref) end,
	    lists:foreach(fun(Cnode) -> constant_test_node_activation(Cnode,Wme_ref) 
			  end,Chn)
    end,
    {noreplay,State};

handle_cast({bm_left_activation,Token_ref,Wme_ref},State) ->
    New_token_ref = runes_kb:make_token(self(),Token_ref,Wme_ref),
    Beta_m = State#rete_node.variant,
    Token_refs = Beat_m#beta_memory.token_refs,
    if Token_refs == [] ->  Flag = true;
       true          ->  Flag = false
    end,
    Beta_m1 = Beta#beta_memory{token_refs = [New_token_ref|Token_refs]},
    Children = State#rete_node.children,
    lists:foreach(fun(Pair)-> left_activation(Pair,New_token_ref,Flag)
		  end,Children),
    State1 = State#rete_node{variant=Beta_m1},
    {noreply,State1};

handle_cast({jn_left_activation,Tr,Flag},State) ->
    Join_part = State#rete_node.variant
    Amem = Join_part#join.amem,   
    Parent = State#rete_node.parent,   %%%%%%type
    Tests = Join_part#join.tests,
    if Flag ->
	    %Ar0 = Join_part#join.nearest_ancestor_with_same_amem,	    
	    relink_to_alpha_memory(Self(),Amem),
	    {ok, Isnil} = is_mem_nil(Amem),
	    if Isnil -> 
		    dislink_to(self(),Parent)  %%% dislink
	    end;
       true ->
	    Isnil = false
    end,
    if not(Isnil) ->
	    {ok, Mem} = quarry_mem(Amem),
	    lists:foreach(fun(Wr) ->
				  Pass_or_no = perform_join_tests(Tests,Tr,Wr),
				  if Pass_or_no ->
					  Children = State#rete_node.children,
					  lists:foreach(fun(Pair) ->
								left_activation(Pair,Tr,Wr,off)
							end,Children)
				  end
			  end,Mem)
    end,
    {noreply,State};

handle_cast({jn_right_activation,Wr,Flag},State) ->
    Join_part = State#rete_node.variant,
    Amem = Join_part#join.amem,   
    {Parent,_} = State#rete_node.parent,    %should be beta_memory type
    Tests = Join_part#join.tests,
    if Flag ->
	    relink_to_beta_memory(self(),Parent),
	    {ok,Isnil} = is_mem_nil(Parent),
	    if Isnil -> 
		    dislink_to(self(),Amem)   %%%dislink
	    end;
       true ->
	    Isnil = false
    end,
    if not(Isnil) ->
	    {ok, Mem} = quarry_mem(Parent),
	    lists:foreach(fun(Tr) ->
				  Pass_or_no = perform_join_tests(Tests,Tr,Wr),
				  if Pass_or_no ->
					Children = State#rete_node.children,
				        lists:foreach(fun(Pair) ->
							      left_activation(Pair,Tr,Wr,off)	
						      end,Children)
				  end
			  end,Mem)
    end,
    {noreply,State};
handle_cast({rn,Node},State0) ->
    Type = type_of(State0),
    case Type of
	{rete_node,_} ->
	    Children0 = State0#rete_node.children,
	    Children1 = lists:delete({Node,Type},Children0),
	    State1 = State0#rete_node{children = Children1},
	    {noreply,State1};    
	am ->
	    Succs0 = Stata0#alpha_memory.succs,
	    Succs1 = lists:delete({Node,Type},Succs0),
	    State1 = State0#alpha_memory{succs = Succs1},
	    {noreply,State1}
    end;
handle_cast(rmd, State) ->	   
    %join
    Type = type_of(State),
    case Type of 
	{rete_node,_} ->
	    Parent = State#rete_node.parent,   %%%%
	    dislink_to(self(),Parent);
	_ ->
	    LOG("no such node as ~p : ~p~n",[Type,self()]),
    end,
    {noreply,State};
handle_cast(d2a,State0) ->
    Type = type_of(State0),
    Succs0 = State0#alpha_memory.succs,
    
    
handle_cast({l2m,Node},State0) ->
    Type = type_of(State0),
    case Type of
	{rete_node,bm} ->
	    Children0 = State0#rete_node.children,
	    Children1 = [{Node,Type}|Children0],
	    State1 = State0#rete_node{children = Children1},
	    {noreply,State1};
	am ->
	    Succs0 = State0#alpha_memory.succs,
	    Ancestor = find_ancestor(Node),
	    Succs1 = insert_after({Node,Type},Ancestor,Succs0),
	    State1 = State0#rete_node{succs =Succs1},	    
	    {noreply,State1};
	_ ->
	    LOG("no such node as ~p : ~p~n",[Type,self()])
	    
    end;
handle_cast({ln,Node},State0) ->
    Type = type_of(State0),
    case Type of
	ctn ->
	    Chn0 = State0#contant_test_ndoe.children,
	    Chn1 = [{Node,Type}|Chn0],
	    State1 = State0#constant_test_node{children = Chn1},
	    {noreply,State1};
	_->
	    LOG("no such node as ~p : ~p~n",[Type,self()]),
	    {noreply,State0}
    end;
handle_cast({rw,Wme_ref},State0) ->
    %%State0=alpha_memory
    Wme_refs0 = State0#alpha_memory.wme_refs,
    Wme_refs1 = lists:delete(Wme_ref,Wme_refs0),
    State1 = State0#alpha_memory{wme_refs = Wme_refs},
    if Wme_refs1 == [] ->
	    Succs = State0#alpha_memory.succs,
	    lists:foreach(fun({Succ,_})->
				  removed(Succ)
			  end,Succs)      
    end,
    {norepy,State1};
handle_cast({rt,Tr},State0) ->
    Type = type_of(State0),
    case Type of
	{rete_node,bm} ->
	    Bm = State0#rete_node.variant,
	    Trs = lists:delete(Tr,Bm#beta_memory.token_refs),
	    Bm1 = Bm#beta_memory(token_refs = Trs),
	    State1 = State0#rete_node{variant=Bm},
	    if Trs == [] ->
		    Chn = State0#rete_node.children,
		    lists:foreach(fun({Ch,_}) ->
					 % Am = Ch#rete_node.variant#join.amem,
					 %dislink_to(Ch,Am)  %%%%%
					  dislink_right(Ch)   %????
				  end,Chn)
	    end,
	    {noreply,State1};
	_ ->
	    LOG("no such node as ~p : ~p~n",[Type,self()]),
	    {noreply,State0}
    end;        

handle_cast() ->	
    
	    
handle_call(is_nil,_From,State) ->
    Type = type_of(State),
    case Type of
	am ->
	    Mem = State#alpha_memory.wme_refs;
	bm ->
	    Mem = State#rete_node.variant#beta_memory.token_refs;
	_ ->
	    LOG("no such node as ~p : ~p~n",[Type,self()]),
	    Bool = no_such_node
    end,
    if Mem == [] ->
	    Bool = true;
       true -> 
	    Bool = false
    end,
    {reply, {ok, Bool}, State};
handle_call(quarry_mem,_From,State) ->
    case type_of(State) of
	am ->
	    Mem = State#alpha_memory.wme_refs;
	bm ->
	    Mem = State#rete_node.variant#beta_memory.token_refs;
	_ ->
	    LOG("no such node as ~p : ~p~n",[Type,self()]),
	    Mem = no_such_node
    end,
    {reply,{ok,Mem}, State};
handel_call({quarry_keys,Keys},_From,State) ->
    Type = type_of(State),
    case Type of
	ctn ->
	    Values = get_values_of_ctn(Keys,State);
	_ ->
	    LOG("no such node as ~p : ~p~n",[Type,self()]),
	    Values = not_match
    end,
    {reply,{ok,Values},State};    
handle_call(is_in_mem,From,State) ->
    Type = type_of(State),
    case Type of
	am ->
	    Succs = State#alpha_memory.succs,
	    Is = lists:member(From,Succs),
	    {reply,{ok,Is},State};   
	_ ->
	    LOG("no such node as ~p : ~p~n",[Type,self()]),
	    {reply,{ok,no_such_node},State}
    end;
handle_call(find_ancestor,_From,State) ->			      
    join#{amem = Amem,nearest_ancestor_with_same_amem = Ancestor} = State#rete_ndoe.variant,
    if Ancestor0 /= nil ->
	    {ok,Y_or_n} = is_right_unlinked(Amem),
	    if Y_or_n ->
		    {ok,Ancestor1} = find_ancestor(Ancestor0),
		    if Ancestor1 == nil ->
			    Return = Ancestor0;
		       true ->
			    Return = Ancesotr1
		    end;
	       true ->
		    Return = Ancestor0
	    end;
       true ->
	    Return = Ancestor0
    end,
    {reply,{ok,Return},State}.

perform_join_tests([],_,_) ->
    true;
perform_join_test([Test|T] = Tests,Token_ref,Wme_ref) ->
    Wme1 = runes_kb:get_wme(Wme_ref),
    #test_at_join_node{field_of_arg1=F1,
		       condition_number_of_arg2=Nth,
		       field_of_arg2=F2} = Test,
    Arg1= runes_kb:get_f(Wme1,F1),
    Wme_ref2 = runes_kb:get_nth_wr_from_token(Token_ref,Nth),
    Wme2 = runes_kb:get_wme(Wme_ref2),
    Arg2 = runes_kb:get_f(Wme2,F2),
    if Arg1 /= Arg2 -> false;
        _           -> perform_join_tests(T,Token_ref,Wme_ref)
    end.
	
type_of(State) when is_record(State,alpha_memory) -> am;
type_of(State) when is_record(State,constant_test_node) -> ctn;
type_of(State) when is_record(State,rete_node) ->
    Variant = State#rete_node.variant,
    type_of(Variant);
type_of(Variant) when is_record(Variant,beta_memory) -> {rete_node,bm};
type_of(Variant) when is_record(Variant,join) -> {rete_node,join}. 

insert_after(Node,P,Ls)->
    case P of
	nil  ->
	    lists:append(Ls,[Node]);
	_ ->
	    {L1,L2} = lists:splitwith(fun(N) ->
					      N /= P
				      end,Ls),
	    L1 ++ [Node] ++ L2
    end.

get_values_of_ctn(Keys,State) ->
    lists:map(fun(Key)->
		      get_value_of_ctn(Key,State)
	      end,Keys).    
			  
get_value_of_ctn(Key,State) ->
    case Key of
	f ->
	    {f,State#constant_test_node.field};
	v ->
	    {v,State#constant_test_node.value};
	o ->
	    {o,State#constant_test_node.out_put_mem};
	c ->
	    {c,State#constant_test_node.children}
    end.
   	%Ancestor = State#rete_node.variant#join.nearest_ancestor_with_same_amem,
						%if Ancestor /= nil ->
						%Amem0 = Ancestor0#rete_node.variant#join.amem,
						% {ok,Is_in} = is_right_unlinked(Ancestor),
						% if Is_in ->
						%	    find_ancestor(Ancestor);
						%      true  ->
						%	    {reply,{ok,Ancestor},State}
						%   end;
						% true ->
						% {reply,{ok,Ancestor},State}
						%end;
    %Ancestor = State#rete_node.variant#join.nearest_ancestor_with_same_amem,
    
			  

    
%handle_call({rmwme,Wme_ref},From,State0) ->
    %%State0=alpha_memory
%    Wme_refs0 = State0#alpha_memory.wme_refs,
%    Wme_refs1 = lists:delete(Wme_ref,Wme_refs0),
%    State1 = State0#alpha_memory{wme_refs = Wme_refs},
%    if Wme_refs1 == [] ->
%	    {reply,{ok,empty},State1};
%       true ->
%	    {reply,{ok,nonempty},State1}
%    end.
	
    
    
    
 
	


%init([alpha_memory, Paras]) ->
%    {alpha_memroy, {wme_refs, Wme_refs}, {succs, Succ}, {rc, Rc}} = Paras,
%    {ok,
%     #alpha_memory{%type = alpha_memory,
%		   wmes = Wme_refs,        %list
%		   succs Succ,   
%		   ref_count = Rc}};
%init([beta_memory, Paras]) ->
%    {beta_memory, {type = beta_memory,
%%%%........

%init([constant_test_node, Paras]) ->
%    {constant_test_node, {field, Fi}, {value, Va}, {out_put_mem, Om}, {children, Chn}}
%	= Paras,
%   {ok,
%     #constant_test_node{%type = constant_test,
%			 field =  Fi,
%			 value = Va,
%			 out_put_mem = Om,  %list
%			 children = Chn}};  %list
%init([join_node,Paras]) ->
%    {join, {type, Type}, {children, Children}, {parent, Parent},
%     {amen, Amen}, {tests, Tests}, {nearest_ancestor_with_same_amem, Nawsa}} = Paras,
%%%............    	    
    






