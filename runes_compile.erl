-module(runes_compile).
-export([]).

-compile(export_all).
-include("internal.hrl").

build_or_share_constant_test_node(Parent,Fi,Va) ->
    {ok,Re} = runes_engine:quarry_child_as(Parent,b),
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
			       parent=[{Parent,Type}]
			       variant = Bm},
	    {ok,Bnew} = runes_engine:create(beta_memory,Paras),
	    runes_engine:b_linkto_p(Bnew,Parent),
	    update_new_node_with_matches_from_above(Bnew),
	    Bnew.

get_join_tests_from_condition(Cond,EConds) ->
    VarL = find_variables_in_cond(Cond),
    EVarLs = lists:map(find_variables_in_cond,EConds),
    
    


    
    
    
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
    update_new_node_with_matches_from_above(P_node).

update_new_node_with_matches_from_above(New) -> %new_node:rete_nde
    {ok,[{p,{Parent,Type}}]}  = rune_engine:quarry_keys(New,[p]),
    case Type of
	bm ->
	    {ok,[{ts,Trs}]} = rune_engine:quarry_keys(Parent,[ts]),
	    lists:foreach(fun(Tr) ->
				  rune_engine:left_activation(New,Tr)
			  end,Trs);
	join ->
	    fake_right_activation(Parent);
	_ ->
	    no_such_match
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
				    {val,_} ->
				       A
				end;
			   true ->
				A
			end,
		end,[],Ls).

quarry_keys(Node,Keys) ->					  
    runes_engine:quarry_keys(Node,Keys).

alpha_memory_activation (Am,Wr) ->
    runes_engine:alpha_memory_activation(Am,Wr).
left_activation(Node,Tr) ->
    runes_engine:left_activation(
create_node(Type,Paras) ->
    runes_engine:create(Type,Paras).



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
