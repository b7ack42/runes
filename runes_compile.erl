-module(runes_compile).
-export([]).

-compile(export_all).
-include("internal.hrl").

build_or_share_constant_test_node(Parent,Fi,Va) ->
    {ok,Re} = runes_engine:quarry_child_as(Parent,c),
    if Re /= nil ->
	    Re;
       true ->
	    Paras = #constant_test_node{field = Fi,
					value = Va,
					out_put_mem = nil,
					children = []},
	    {ok,New} = runes_engine:create_node(constant_test_node,Paras),
	    runes_engine:c_link_pc(New,Parent),
	    New
    end.

build_or_share_alpha_memory(Conds)->
    Cur_n = lists:foldl(fun({F,V} = Cond,Cur)->
				build_or_share_constant_test_node(Cur,F,V),
			end, dummy_top_node,Conds),
    {ok,[{o,Om}]} = runes_engine:quarry_keys(Cur_n,[o])
    if Om /= nil ->
	    Om;
       true ->
	    Paras = #alpha_memory{succ = [],
				  ref_count = 0,
				  wme_refs = []},
	    {ok,Am} = runes_engine:create(alpha_memory,Paras),
	    runes_engine:a_linkto_pc(Am,Cur_n),
	    Wrs = runes_kb:get_all_wme_refs(),
	    lists:foreach(fun(Wr) ->
				  Pass_or_no = perform_test(Wr,Conds),
				  runes_engine:alpha_memory_activation(Am,Wr)
			  end,Wrs),
	    Am		
    end.   
	
build_or_share_beta_memory_node({Parent,Type}) ->
    {ok,Ch} = runes_engine:quarry_child_as(Parent,b),
    if Ch /= nil
       ->
	    Ch;
       true ->
	    Bm = #beta_memory{token_refs=[],all_children=[]},
	    Paras = #rete_node{type = bm,
			       children=[],
			       parent={Parent,_Type}
			       variant = Bm},
	    {ok,Bnew} = runes_engine:create(beta_memory,Paras),
	    runes_engine:b_linkto_p(Bnew,Parent),
	    update_new_node_with_matches_from_above({Bnew,b}),
	    Bnew
    end.

get_join_tests_from_condition(Cond,EConds) ->
    VarL = find_variables_in_cond(Cond),
    EVarLs = lists:reverse(lists:map(fun(C)->
					     find_variables_in_cond(C)
				     end,EConds)),
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
	    find_test(VF,T);
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
    if Nt == dummy_top_node ->
	    nil;
       true ->
	    {Node,Type} = Nt,
	    case Type of
		j ->
		   {ok,[{a,Amm}]} = runes_engine:quarry_keys(Nt,[a]),
		    if Amm == Am ->
			    Nt;
		       true ->
			    nil
		    end;
		true ->
		    nil
	    end
    end.

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
	    {ok,Am_nil} = runes_engine:is_mem_nil(Am),
	    {ok,Bm_nil} = runes_engine:is_mem_nil(Parent),
	    runes_engine:j_linkto_p(New,Parent,Am_nil)
	    end,
	    if not(Bm_nil) ->
		    runes_engine:j_linkto_a(New,Am)
	    end,
	    New
    end.
	    
	    
build_or_share_network_for_conditions(Pa,Conds,EConds) ->   
    {Cur_n, _} = lists:foldl(one_step_build,{Pa,EConds},Conds),    
    Cur_n.

one_step_build(Cond,{Cur_n,Conds_h}) ->
    case Cond of
	positive ->
	    Cur_nn = build_or_share_beta_memory_node(Cur_n),
	    Tests = get_join_tests_from_condition(Cond,Conds_h),
	    Am = build_or_share_join_node(Cond),
	    Cur_nnn = build_or_share_join_node(Cur_nn,Am,Tests),
	    Conds_hh = [Cond|Conds_h],
	    {Cur_nnn,Conds_hh};
	_ ->
	    {fail,no_such_cond}
    end.

add_production(Lhs) ->
    Cur_n = build_or_share_network_for_conditions(dummy_top_node,Lhs,nil),
    P_node = build_p_node(Cur_n),
    update_new_node_with_matches_from_above({P_node,pn}).

update_new_node_with_matches_from_above(New) -> %new_node:rete_nde
    {Node,_Type} = New,
    {ok,[{p,{Parent,Type}}]}  = rune_engine:quarry_keys(New,[p]),
    case Type of
	b ->
	    {ok,[{t,Trs}]} = rune_engine:quarry_keys(Parent,[t]),
	    lists:foreach(fun(Tr) ->
				  rune_engine:left_activation(New,Tr)
			  end,Trs);
	j ->
	    runes_engine:fake_right_activation(Parent,New);
	_ ->
	    no_such_match
    end.

remove_production(Prod) ->
    Pn = runes_engine:get_pn(Prod),
    delete_node_and_any_unused_ancestors(Pn).

delete_node_and_any_unused_ancestors(N) ->
    runes_engine:delete(N).

delete_alpha_memor(Am) ->    
    runes_engine:delete(Am)
    
	    
find_variables_in_cond(Cond) ->
    %Cond = fields
    #fields{id=Id,attr=Attr,value=Val} = Cond,
    Ls = [{Id,id},{Attr,attr},{Val,value}],
    lists:foldr(fun({V,F},A) ->
			if V /= nil ->
				case V of
				    {var,Var} ->
					[{Var,F}|A];
				    {val,_} ->
				       A
				end;
			   true ->
				A
			end
		end,[],Ls).


perform_test(_,[]) -> true;
perform_test(Wr,[{F,V}|Cs])->
    Wme = runes_kb:get_wme(Wr),
    Fields = wme#wme.fields,
    Bool = 
	case F of
	    id -> 
		Fields#fields.id == V;
	    attr ->
		Field#fields.attr == V;
	    value ->
		Field#fields.value == V
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

