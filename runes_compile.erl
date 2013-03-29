-module(runes_compile).
-export([]).

-compile(export_all).
-include("internal.hrl").

build_or_share_constant_test_node(Parent,Fi,Va) ->
    Pred = fun(Ctn) ->
		   {ok,KVs} = runes_engine:quarry_keys(Ctn,[field,value]),
		   [{f,F},{v,V}] = KVs,
		   F == Fi andalso Va == V
	   end,
   [{c,PChildren}] = ruens_engine:quarry_keys(Parent,[c]),
    {ok,Re} = take_if(Pred,PChildren),
    if Re /= nil ->
	    Re;
       true ->
	    Paras = #constant_test_node{field = Fi,
					value = Va,
					out_put_mem = nil,
					children = nil},
	    {ok,New} = runes_engine:create(constant_test_node,Paras),
	    runes_engine:link_to(New,Parent),
	    New
    end.

build_or_share_alpha_memory(Conds)->
    Cur_n = lists:foldl(fun({F,V} = Cond,Cur)->
				build_or_share_constant_test_node(Cur,F,V),
			end, root_node,Conds),
    Om = Cur_n#constant_test_node.out_put_mem,
    if Om /= nil ->
	    Om;
       true ->
	    Paras = #alpha_memory{succ = [],
				  ref_count = 0,
				  wme_refs = []},
	    {ok,Am} = runes_engine:create(alpha_memory,Paras),
	    Wrs = runes_kb:get_all_wme_refs(),
	    lists:foreach(fun(Wr) ->
				  Pass_or_no = perform_test(Wr,Conds),
				  runes_engine:alpha_memory_activation(Am,Wr)
			  end,Wrs)
    end.   
	
build_or_share_beta_memory_node(Parent,PChn) -> 
    
    
				       
					  
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
