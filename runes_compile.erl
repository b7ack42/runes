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

build_or_share_constant_test_node(root_node,Fi,Va) ->
    {ok,Root} = runes_agenda:get_root(),
    build_or_share_constant_test_node(Root,Fi,Va);
build_or_share_constant_test_node(Parent,Fi,Va) ->
    {Tag,Val} = Va,
    if Tag == con ->
	    {ok,Re} = runes_engine:quarry_child_as(Parent,{c,Fi,Val}),
	    if Re /= nil ->
		    Re;
	       true ->
		    Paras = #constant_test_node{field = Fi,
						value = Val,
						out_put_mem = nil,
						parent = Parent,
						children = []},
		    {ok,New} = runes_engine:create(constant_test_node,Paras),
		    runes_engine:c_linkto_pc(New,Parent),
		    runes_agenda:inc_node_num(ctn),
		    New
	    end;
       true ->
	    Parent
    end.

build_or_share_alpha_memory(Cond)->
    #fields{id=Id,attr=Attr,value=Val} = Cond,
    Ls = [{id,Id},{attr,Attr},{value,Val}],
    Cur_n = lists:foldl(fun({F,V},Cur)->
				build_or_share_constant_test_node(Cur,F,V)
			end, root_node,Ls),
    {ok,[{o,Om}]} = runes_engine:quarry_keys(Cur_n,[o]),
    if Om /= nil ->
	    Om;
       true ->
	    Paras = #alpha_memory{succs = [],
				  parent = Cur_n,
				  ref_count = 0,
				  wme_refs = []},
	    {ok,Am} = runes_engine:create(alpha_memory,Paras),
	    runes_engine:a_linkto_pc(Am,Cur_n),
	    Wrs = runes_kb:get_all_wme_refs(),
	    lists:foreach(fun(Wr) ->
				  Pass_or_no = perform_test(Wr,Cond),
				  if Pass_or_no ->
					  runes_engine:alpha_memory_activation(Am,Wr)
				  end
			  end,Wrs),
	    runes_agenda:inc_node_num(am),
	    Am		
    end.   

%build_or_share_beta_memory_node(dummy_top_node) ->
%    Bm = #beta_memory{token_refs=[nil],all_children=[]},
%    Paras = #rete_node{type = bm,
%		       children = [],
%		       parent = nil,
%		       variant = Bm},
%    {ok,Bn} = runes_engine:create(beta_memory,Paras),
%    {Bn,b};
%build_or_share_beta_memory_node(dummy_top_node)->
%    dummy_top_node;
build_or_share_beta_memory_node(dummy_top_node) ->
    {ok,Dn} = runes_agenda:get_dummy_top_node(),
    {ok,Ch} = runes_engine:quarry_child_as(Dn,b),
    if Ch /= nil ->
	    Ch;
       true ->
	    Bm = #beta_memory{token_refs=[nil],all_children=[]},
	    Paras = #rete_node{type = bm,
			       children=[],
			       parent={Dn,d},
			       variant = Bm},
	    {ok,Bnew} = runes_engine:create(beta_memory,Paras),
	    runes_engine:b_linkto_p(Bnew,Dn),
	    runes_agenda:inc_node_num(bm),
	    {Bnew,b}
    end;
build_or_share_beta_memory_node({Parent,Type}) ->
    {ok,Ch} = runes_engine:quarry_child_as(Parent,b),
    if Ch /= nil ->
	    Ch;
       true ->
	    Bm = #beta_memory{token_refs=[],all_children=[]},
	    Paras = #rete_node{type = bm,
			       children=[],
			       parent={Parent,Type},
			       variant = Bm},
	    {ok,Bnew} = runes_engine:create(beta_memory,Paras),
	    runes_engine:b_linkto_p(Bnew,Parent),
	    update_new_node_with_matches_from_above({Bnew,b}),
	    runes_agenda:inc_node_num(bm),
	    {Bnew,b}
    end.

build_or_share_p_node({Pa,Type},Rule) ->
    #rule{name = Na,rhs= Rhs} = Rule,
    {ok,Ch} = runes_engine:quarry_child_as(Pa,{p,Na}),
    if Ch /= nil ->
	    Ch;
       true ->
	    Paras = #p_node{parent = {Pa,Type},
			    rule_name= Na,
			    token_refs = [],
			    action = Rhs},
	    {ok,Pn} = runes_engine:create(p_node,Paras),
	    runes_engine:pn_linkto_p(Pn,Pa),
	    runes_agenda:put_pn(Na,Pn),
	    runes_agenda:inc_node_num(pn),
	    {Pn,p}
    end.
	    
find_variables_in_cond(Cond) ->
    %Cond = fields
    #fields{id=Id,attr=Attr,value=Val} = Cond,
    Ls = [{Id,id},{Attr,attr},{Val,value}],
    lists:foldr(fun({V,F},A) ->
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
		end,[],Ls).

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

%build_or_share_join_node(dummy_top_node,Am,Tests)->
%    Jn=#join{amem=Am,
%	     tests = Tests,
%	     nearest_ancestor_with_same_amem = nil},
%    Paras = #rete_node{type=join,
%		       children=[],
%		       parent = dummy_top_node,
%		       variant=Jn},
%    {ok,New} = runes_engine:create(join,Paras),
%    runes_engine:j_linkto_a(New,Am,false),
%    {New,j};
build_or_share_join_node({Parent,Type},Am,Tests) ->
    {ok,Ch} = runes_engine:quarry_child_as(Parent,{j,Am,Tests}),
    if Ch /= nil ->
	    Ch;
       true ->
	    Na=find_nearest_ancestor_with_same_amem({Parent,Type},Am),
	    Jn = #join{amem = Am,
		       tests = Tests,
		       nearest_ancestor_with_same_amem = Na},
	    Paras = #rete_node{type = join,
			       children = [],
			       parent = {Parent,Type},
			       variant = Jn},
	    {ok,New} = runes_engine:create(join,Paras),
	    {ok,Bm_nil} = runes_engine:is_mem_nil(Parent),
	    if Bm_nil ->
		    Am_nil = false;
	       true ->
		    {ok,Am_nil} = runes_engine:is_mem_nil(Am)
	    end,
	    runes_engine:j_linkto_p(New,Parent,Am_nil),
	    runes_engine:j_linkto_a(New,Am,Bm_nil),
	    runes_agenda:inc_node_num(jn),
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
	    Cur_nn = build_or_share_beta_memory_node(Cur_n),
	    {ok,Tests} = get_join_tests_from_condition(Cond,Conds_h),
	    Am = build_or_share_alpha_memory(Cond),
	    Cur_nnn = build_or_share_join_node(Cur_nn,Am,Tests),
	    Conds_hh = [Cond|Conds_h],
	    {Cur_nnn,Conds_hh}.
%	_ ->
%	    {fail,no_such_cond}
 %   end.


    
    
add_production(Rule) ->
    Lhs =  Rule#rule.lhs,
    Cur_n = build_or_share_network_for_conditions(dummy_top_node,Lhs,[]),
    P_node = build_or_share_p_node(Cur_n,Rule),
    update_new_node_with_matches_from_above(P_node).

update_new_node_with_matches_from_above(New) -> 
    {Node,_Type} = New,
    {ok,[{p,{Parent,Type}}]}  = runes_engine:quarry_keys(Node,[p]),
    case Type of
	b ->
	    {ok,[{t,Trs}]} = runes_engine:quarry_keys(Parent,[t]),
	    lists:foreach(fun(Tr) ->
				  runes_engine:left_activation(New,Tr,nil,off)
			  end,Trs);
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
    Bool = 
	case F of
	    id -> 
		Fields#fields.id == V;
	    attr ->
		Fields#fields.attr == V;
	    value ->
		Fields#fields.value == V
	end,
    if Bool ->
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

