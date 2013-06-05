-module(runes_compile).
-export([add_production/1,
	 remove_production/1
	]).

-compile(export_all).
-include("internal.hrl").

build_root_node_only_once() ->
    Paras = #root_node{children = []},
    {ok,Root} = runes_engine:create(root_node,Paras),
    Root.

build_dummy_top_node_only_once() ->
    Paras = #rete_node{type=dtn,parent=nil,children = [],variant=dummy_top_node},
    {ok,Dn} = runes_engine:create(dummy_top_node,Paras),
    Dn.

build_or_share_constant_test_node(root_node,Fi,Va,_Where) ->
    {_,Class} = Va,
    Wh = runes_kb:find_class(Class),
    if Wh /= no_class ->
	    Where = Wh;
       true ->
	    [_F|T] = nodes(),
	    Nodes = [node()|T],
	    Where = lists:nth(random:uniform(length(Nodes)),Nodes)
    end,
    {ok,Root} = runes_agenda:get_root_and_set_class(Where,Class),
    build_or_share_constant_test_node(Root,Fi,Va,Where);
build_or_share_constant_test_node(Parent,Fi,Va,Where) ->
    {Tag,Val} = Va,
    if Tag == con ->
	    {ok,Re} = runes_engine:quarry_child_as(Parent,{c,Fi,Val}),
	    if Re /= nil ->
		    {Re,Where};
	       true ->
		    Paras = #constant_test_node{field = Fi,
						value = Val,
						out_put_mem = nil,
						parent = Parent,
						children = []},
		    %{ok,New} = runes_engine:create(constant_test_node,Paras),
		    {ok,New} = runes_agenda:create_node(Where,
							ctn,
							Paras),
		    runes_engine:c_linkto_pc(New,Parent),
		    %runes_agenda:inc_node_num(ctn),
		    {New,Where}
	    end;
       true ->
	    {Parent,Where}
    end.

build_or_share_alpha_memory(Cond,_Where)->
    %#fields{id=Id,attr=Attr,value=Val} = Cond,
    %Ls = [{id,Id},{attr,Attr},{value,Val}],
    {Cur_n,Where} = lists:foldl(fun({F,V},{Cur,Where})->
				build_or_share_constant_test_node(Cur,F,V,Where)
			end, {root_node,nil},Cond),
    {ok,[{o,Om}]} = runes_engine:quarry_keys(Cur_n,[o]),
    if Om /= nil ->
	    {Om,Where};
       true ->
	    Paras = #alpha_memory{succs = [],
				  parent = Cur_n,
				  ref_count = 0,
				  wme_refs = []},
	    %{ok,Am} = runes_engine:create(alpha_memory,Paras),
	    {ok,Am} = runes_agenda:create_node(Where,am,Paras),
	    runes_engine:a_linkto_pc(Am,Cur_n),
	    Wrs = runes_kb:get_all_wme_refs(),
	    lists:foreach(fun(Wr) ->
				  Pass_or_no = perform_test(Wr,Cond),
				  if Pass_or_no ->
					  runes_engine:alpha_memory_activation(Am,Wr)
				  end
			  end,Wrs),
	    %runes_agenda:inc_node_nSum(am),
	   {Am,Where}		
    end.   

build_or_share_beta_memory_node(dummy_top_node,Where) ->
    {ok,Dn} = runes_agenda:get_dummy_top_node(Where),
    {ok,Ch} = runes_engine:quarry_child_as(Dn,b),
    if Ch /= nil ->
	    Ch;
       true ->
	    Bm = #beta_memory{token_refs=[nil],all_children=[]},
	    Paras = #rete_node{type = bm,
			       children=[],
			       parent={Dn,d},
			       variant = Bm},
%	    {ok,Bnew} = runes_engine:create(beta_memory,Paras),
	    {ok,Bnew} = runes_agenda:create_node(Where,bm,Paras),
	    runes_engine:set_bm(Bnew,[nil]),
	    runes_engine:b_linkto_p(Bnew,Dn),
	    %runes_agenda:inc_node_num(bm),
	    {Bnew,b}
    end;
build_or_share_beta_memory_node({Parent,Type},Where) ->
    {ok,Ch} = runes_engine:quarry_child_as(Parent,b),
    if Ch /= nil ->
	    Ch;
       true ->
	    Bm = #beta_memory{token_refs=[],all_children=[]},
	    Paras = #rete_node{type = bm,
			       children=[],
			       parent={Parent,Type},
			       variant = Bm},
	    %{ok,Bnew} = runes_engine:create(beta_memory,Paras),
	    {ok,Bnew} = runes_agenda:create_node(Where,bm,Paras),
	    %Name = list_to_atom(pid_to_list(Bnew)),
	    %ets:new(Name,[public,named_table,{read_concurrency,true}]),
	    runes_engine:b_linkto_p(Bnew,Parent),
	    update_new_node_with_matches_from_above({Bnew,b}),
	    %runes_agenda:inc_node_num(bm),
	    {Bnew,b}
    end.

build_or_share_p_node({Pa,Type},Rule,Where) ->
    #rule{name = Na,rhs= Rhs} = Rule,
    {ok,Ch} = runes_engine:quarry_child_as(Pa,{p,Na}),
    if Ch /= nil ->
	    Ch;
       true ->
	    Paras = #p_node{parent = {Pa,Type},
			    rule_name= Na,
			    token_refs = [],
			    action = Rhs},
	   % {ok,Pn} = runes_engine:create(p_node,Paras),
	    {ok,Pn} = runes_agenda:create_node(Where,pn,Paras),
	    runes_engine:pn_linkto_p(Pn,Pa),
	    runes_agenda:put_pn(Na,Pn),
	    %runes_agenda:inc_node_num(pn),
	    {Pn,p}
    end.
	    
find_variables_in_cond(Cond) ->
    %Cond = fields
    %$#fields{id=Id,attr=Attr,value=Val} = Cond,
    %$Ls = [{Id,id},{Attr,attr},{Val,value}],
    lists:foldr(fun({F,V},A) ->
			if V /= nil ->
				case V of
				    {var,Var} ->
					[{Var,F}|A];
				    {con,_} ->
				       A
				end;
			   true ->
				A
			end
		end,[],Cond).

get_join_tests_from_condition(Cond,EConds) ->
    VarL = find_variables_in_cond(Cond),
    EVarLs = lists:map(fun(C)->
			       find_variables_in_cond(C)
		       end,EConds),
    Tests = lists:filter(fun(T) ->
				 T /= nil
			 end,lists:map(fun(VF) ->
					       find_test(VF,EVarLs,0)
				       end,VarL)),
    {ok,Tests}.

find_test(_,[],_) -> nil;    
find_test(VF,[Vl|T],Pos) -> 
    Re = find_test_help(VF,Vl),
    if Re == {fail,nil} ->
	    find_test(VF,T,Pos+1);
       true ->
	    {ok,F1,F2} = Re,
	    #test_at_join_node{field_of_arg1 = F1,
			       condition_number_of_arg2 = Pos,
			       field_of_arg2 = F2}
    end.

find_test_help(_,[]) -> {fail,nil};
find_test_help({V1,F1},[{V2,F2}|VFs]) ->
    if V1==V2 ->
	    {ok,F1,F2};
       true ->
	    find_test_help({V1,F1},VFs)
    end.

find_nearest_ancestor_with_same_amem(Nt,Am)->
    if Nt == nil ->   % dummy_top_node ->
	    nil;
       true ->
	    {Node,Type} = Nt,
	    case Type of
		j ->
		   {ok,[{a,Amm}]} = runes_engine:quarry_keys(Node,[a]),
		    if Amm == Am ->
			    Node;
		       true ->
			    nil
		    end;
		b ->
		    {ok,[{p,Pa}]} = runes_engine:quarry_keys(Node,[p]),
		    find_nearest_ancestor_with_same_amem(Pa,Am);
		d->
		    nil;
		_Else ->
		    no_such_type_in_fnawsa
	    end
    end.

build_or_share_join_node({Parent,Type},Am,Tests,Where) ->
    {ok,Ch} = runes_engine:quarry_child_as(Parent,{j,Am,Tests}),
    if Ch /= nil ->
	    Ch;
       true ->
%	    Na=find_nearest_ancestor_with_same_amem({Parent,Type},Am),
	    Na = nil,
	    Jn = #join{amem = Am,
		       tests = Tests,
		       nearest_ancestor_with_same_amem = Na},
	    Paras = #rete_node{type = join,
			       children = [],
			       parent = {Parent,Type},
			       variant = Jn},
	    %{ok,New} = runes_engine:create(join,Paras),
	    {ok,New} = runes_agenda:create_node(Where,jn,Paras),
	    %Bm_nil = runes_engine:is_bm_nil(Parent),
	    %if Bm_nil ->
	%	    Am_nil = false;
	 %      true ->
	%	    Am_nil = runes_engine:is_am_nil(Am)
	 %   end,
	    Bm_nil = false,
	    Am_nil = false,
	    runes_engine:j_linkto_p(New,Parent,Am_nil),
	    runes_engine:j_linkto_a(New,Am,Bm_nil),
	    %runes_agenda:inc_node_num(jn),
	    {New,j}
    end.
	
	    
build_or_share_network_for_conditions(Pa,Conds,EConds) ->   
    {Cur_n, _} = lists:foldl(fun(C,CC) ->
				     one_step_build(C,CC)
			     end,{Pa,EConds},Conds),    
    Cur_n.

one_step_build(Cond,{Cur_n,Conds_h}) ->
   % case Cond of
%	positive ->
%    [_|T] = nodes(),
%    Nodes = [node()|T],
%    Node = lists:nth(random:uniform(lenght(Nodes)),Nodes),
    {Am,Where} = build_or_share_alpha_memory(Cond,wherenil),
    Cur_nn = build_or_share_beta_memory_node(Cur_n,Where),
    {ok,Tests} = get_join_tests_from_condition(Cond,Conds_h),
    Cur_nnn = build_or_share_join_node(Cur_nn,Am,Tests,Where),
    Conds_hh = [Cond|Conds_h],
    {Cur_nnn,Conds_hh}.
%	_ ->
%	    {fail,no_such_cond}
 %   end.


    
    
add_production(Rule) ->
    random:seed(erlang:now()),
    Lhs =  Rule#rule.lhs,
    Cur_n = build_or_share_network_for_conditions(dummy_top_node,Lhs,[]),
    [_|T] = nodes(),
    Nodes = [node()|T],
    P_node = build_or_share_p_node(Cur_n,Rule,lists:nth(random:uniform(length(Nodes)),
							Nodes)),
    update_new_node_with_matches_from_above(P_node).

update_new_node_with_matches_from_above(New) -> 
    {Node,_Type} = New,
    {ok,[{p,{Parent,Type}}]}  = runes_engine:quarry_keys(Node,[p]),
    case Type of
	b ->
	    %{ok,[{t,Trs}]} = runes_engine:quarry_keys(Parent,[t]),
	    Trs = runes_engine:quarry_bm(Parent),
	    Tl = lists:map(fun(Tr) ->
				   runes_kb:get_token(Tr)
			   end,Trs),
	    %$lists:foreach(fun(Tr) ->
				  runes_engine:left_activation(New,{Tl,0},off);
	    %$		  end,Trs);
	j ->
	    runes_engine:fake_right_activation(Parent,New);
	_ ->
	    no_such_match
    end.

remove_production(Prod) ->
    Rn = Prod#rule.name,
    Pn = runes_agenda:get_pn(Prod),
%    delete_node_and_any_unused_ancestors(Pn).
    runes_engine:delete(Pn),
    runes_agenda:delete_rn(Rn).

%delete_node_and_any_unused_ancestors(N) ->
%    runes_engine:delete(N).

%delete_alpha_memory(Am) ->    
%    runes_engine:delete(Am).
    


perform_test(_,[]) -> true;
perform_test(Wr,[{F,V}|Cs])->
    Wme = runes_kb:get_wme(Wr),
    Fields = Wme#wme.fields,
    Va = proplists:get_value(F,Fields),
%    Bool = 
%	case F of
%	    id -> 
%		Fields#fields.id == V;
%	    attr ->
%		Fields#fields.attr == V;
%	    value ->
%		Fields#fields.value == V
%	end,
 %   if Bool ->
    if Va == V ->
	    perform_test(Wr,Cs);
       true ->
	    false
    end.		   

take_if(Pred,[]) -> {ok,nil};
take_if(Pred,[T|Ts]) ->
    Bool = apply(Pred,[T]),
    if Bool -> {ok,T};
       true -> take_if(Pred,Ts)
    end.

