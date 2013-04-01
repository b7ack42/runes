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
    gen_server:start_link(?MODULE, [join_node, Paras], []).

    


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
	j ->
	    join_node_left_activation(Node,Tr,Tag);
	b -> 
	    beta_memory_left_activation(Node,Tr);
	_ -> 
	    LOG("no such node as ~p : ~p",[Type,self()])
    end.

right_activation({Node,Type},Wr,Tag) ->
    case Type of
	j ->
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

relink_to_alpha_memory(Node,Amem)->               %join or negative
    gen_server:cast(Amem,{rl,Node}).  %relink
relink_to_beta_memory(JNode,Bmem) ->
    gen_server:cast(Bmem,{rl,JNode}).  %relink
    
%link_to(Node,Tnode) ->
%    gen_server:cast(Tnode,{ln,Node}).

dislink_to(Node,FNode) ->
    gen_server:cast(FNode,{dl,Node}).
dislink_left(Node) ->
    gen_server:cast(Node,dll).
dislink_right(Node) ->
    gen_server:cast(Node,dlr).

remove(wme,Wme_ref,Amem) ->
    gen_server:cast(Amem,{rw,Wme_ref});
remove(token,Token_ref,Node) ->
    gen_server:cast(Node,{rt,Token_ref}).








quarry_mem(Node) ->
    gen_server:call(Node,qm).
quarry_keys(Node,Keys) ->
    gen_server:call(Node,{qks,Keys}).
quarry_child_as(Node,As) ->
    gen_server:call(Node,{qc,As}).
is_mem_nil(B_or_A) ->		
    gen_server:call(B_or_A,is_nil).

find_ancestor(JNode) ->
    gen_server:call(JNode,find_ancestor).

is_right_unlinked(Amem) ->
    {ok,Is_ru} = gen_server:call(Amem,rup),
    {ok,not(Is_ru)}.


c_linkto_pc(Cn,Pn) ->
    gen_server:cast(Pn,{c2c,Cn}).
a_linkto_pc(Am,Pn) ->
    gen_server:cast(Pn,{a2c,Am}).
b_linkto_p(Bm,Pn) ->
    gen_server:cast(Pn,{b2p,Bm}).
handle_cast({c2c,Cn},State0) ->
    %state = Cnode
    Chn = [Cn|State0#constant_test_node.children],
    State1 = State0#constant_test_node{children = Chn},
    {noreply,State1};
handle_cast({a2c,Am},State) ->
    {noreply,State#constant_test_node{out_put_mem = Am}};
handle_cast({b2p,Bm},State) ->
    Chn = State#rete_node.children,
    {noreply,State#rete_node{children = [{Bm,b}|Chn]}};    
handle_cast() ->





handle_cast({aa,Wme_ref},State) ->
    #alpha_memory{wme_refs = Wme_refs0, succs = Succs} = State,
    Wme_refs1 = [Wme_ref|Wmes_ref0],
    if Wme_refs0 == [] -> Flag = true;
       true            -> Flag = false
    end,
    Wme0 = runes_kb:get_wme(Wme_ref),
    
    #wme{alpha_mems = Alpha_mems0} = Wme0,
    Wme1 = Wme0#wme{alpha_mems = [self()|Alpha_mems0]},
    runes_kb:insert(wm,Wme_ref,Wme1),
    lists:foreach(fun(Pair)->
 			  right_activation(Pair,Wme_ref,Flag)
 		  end,Succs),
    {noreply, State#alpha_memory{wmes=Wme_refs1}};

handle_cast({ca,Wme_ref}, State) ->
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

handle_cast({bla,Token_ref,Wme_ref},State) ->
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

handle_cast({jla,Tr,Flag},State) ->
    Join_part = State#rete_node.variant
    Amem = Join_part#join.amem,   
    {Bm,b} = State#rete_node.parent,   %%%%%%type
    Tests = Join_part#join.tests,
    if Flag ->
	    relink_to_alpha_memory({Self(),j},Amem),
	    {ok, Isnil} = is_mem_nil(Amem),
	    if Isnil -> 
		    dislink_to(self(),Bm)  %%% dislink
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

handle_cast({jra,Wr,Flag},State) ->
    Join_part = State#rete_node.variant,
    Amem = Join_part#join.amem,   
    {Bm,j} = State#rete_node.parent,    %should be beta_memory type
    Tests = Join_part#join.tests,
    if Flag ->
	    relink_to_beta_memory(self(),),
	    {ok,Isnil} = is_mem_nil(Bm),
	    if Isnil -> 
		    dislink_to(self(),Amem)   %%%dislink
	    end;
       true ->
	    Isnil = false
    end,
    if not(Isnil) ->
	    {ok, Mem} = quarry_mem(Bm),
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
handle_cast({dl,Node},State0) ->   %join_node dislink from bm or am
    Type = type_of(State0),
    case Type of
	b ->
	    Children0 = State0#rete_node.children,
	    Children1 = lists:delete({Node,j},Children0),
	    State1 = State0#rete_node{children = Children1},
	    {noreply,State1};    
	a ->
	    Succs0 = Stata0#alpha_memory.succs,
	    Succs1 = lists:delete(Node,Succs0),  %Node = {node,type}
	    State1 = State0#alpha_memory{succs = Succs1},
	    {noreply,State1}
    end;
handle_cast(dll, State) ->	   
    %State should be join
    {Bm,b} = State#rete_node.parent,   %%%%
    dislink_to(self(),Bm);
    {noreply,State};
handle_cast(dlr,State) ->
    Type = type_of(State),
    case Type of 
	j ->    % join
	    Am = State0#rete_node.variatn#join.amem,
	    dislink_to({self(),Type},Am);
	_ -> 
	    LOG("no_such_drl~p~p~n",Type)
    end;
handle_cast({rl,Node},State0) ->
    Type = type_of(State0),
    case Type of
	b ->
	    Children0 = State0#rete_node.children,
	    Children1 = [{Node,j}|Children0],
	    State1 = State0#rete_node{children = Children1},
	    {noreply,State1};
	a ->
	    Succs0 = State0#alpha_memory.succs,
	    Ancestor = find_ancestor(Node),  %Node = {node,type}
	    Succs1 = insert_after(Node,Ancestor,Succs0),
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
	    Chn1 = [Node|Chn0],
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
	    lists:foreach(fun({Succ,Type})->
				  if Type == j ->
					  dislink_left(Succ)
				  end
			  end,Succs)      
    end,
    {norepy,State1};
handle_cast({rt,Tr},State0) ->
    Type = type_of(State0),
    case Type of
	b ->
	    Bm = State0#rete_node.variant,
	    Trs = lists:delete(Tr,Bm#beta_memory.token_refs),
	    Bm1 = Bm#beta_memory(token_refs = Trs),
	    State1 = State0#rete_node{variant=Bm1},
	    if Trs == [] ->
		    ChnT = State0#rete_node.children,
		    lists:foreach(fun({Ch,_}) ->
					  dislink_right(Ch)   %????
				  end
				  end,ChnT)
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
	a ->
	    Mem = State#alpha_memory.wme_refs;
	b ->
	    Mem = State#rete_node.variant#beta_memory.token_refs;
	_ ->
	    LOG("no such node as ~p : ~p~n",[Type,self()]),
	    Nil = no_such_node
    end,
    if Mem == [] ->
	    Nil = true;
       true -> 
	    Nil = false
    end,
    {reply, {ok, Nil}, State};
handle_call(qm,_From,State) ->
    case type_of(State) of
	a ->
	    Mem = State#alpha_memory.wme_refs;
	b ->
	    Mem = State#rete_node.variant#beta_memory.token_refs;
	_ ->
	    LOG("no such node as ~p : ~p~n",[Type,self()]),
	    Mem = no_such_node
    end,
    {reply,{ok,Mem}, State};
handel_call({qks,Keys},_From,State) ->
    Type = type_of(State),
    case Type of
	ctn ->
	    Values = get_values_of_ctn(Keys,State);
	bm ->
	    Values = get_values_of_bm(Keys,State);
	_ ->
	    LOG("no such node as ~p : ~p~n",[Type,self()]),
	    Values = not_match
    end,
    {reply,{ok,Values},State};   
handle_call({qc,c},_From,State) ->
    Chn = State#constant_test_node.children,
    Pred = fun(Ch) ->
		   {ok,KVs} = quarry_keys(Ct,[f,v]),
		   [{f,F},{v,V}] = KVs,
		   F == Fi andalso Va == V
	   end,
     {ok,Re} = take_if(Pred,Chn),
    {reply,{ok,Re},State};    
handle_call({qc,b},_From,State) ->
    Chn = State#rete_node.children,
    Pred = fun({Ch,Type}) ->
		   Type == b
	   end,
    {ok,Re} = take_if(Pred,Chn),
    {reply,{ok,Re},State};
handle_call() ->

    
	
handle_call(rup,From,State) ->  %State should be am
    Succs = State#alpha_memory.succs,
    Is = lists:member({From,j},Succs),
    {reply,{ok,Is},State};   
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
	    L1 ++ [Node|L2]
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

get_values_of_
 


take_if(Pred,[]) -> {ok,nil};
take_if(Pred,[T|Ts]) ->
    Bool = apply(Pred,[T]),
    if Bool -> {ok,T};
       true -> take_if(Pred,Ts)
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
    






