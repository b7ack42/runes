-module(runes_match).

-include("internal.hrl").

-export([
	 group_wrs/2,
	 lperform_join_tests/3,
	 rperform_join_tests/3,
	 merge/2	 
	]).

-compile(export_all).


group_wrs(Wrl,Field) ->
    KeyWrL = lists:map(fun(Wr) ->
			       Wme=runes_kb:get_wme(Wr),
			       {runes_kb:get_f(Wme,Field),Wr}
		       end,Wrl),
    Keys = lists:reverse(proplists:get_keys(KeyWrL)),
    Re = 
	lists:map(fun(Key) ->
			  {Key,proplists:get_all_values(Key,KeyWrL)}
		  end,Keys),
    {Keys,Re}.

split_t(T,F,N) ->
    {L1,[Wrl|L2]} = lists:split(N,T),
    {_,Grouped_wrll} = group_wrs(Wrl,F),
    lists:map(fun({Field,Gwl}) ->
		      {Field,L1++[Gwl|L2]}
	      end,Grouped_wrll).

split_ts(Tl,F,N) ->
    lists:flatmap(fun(T)->
			  split_t(T,F,N)
		  end,Tl).


split_match(Test,Tl) ->
    #test_at_join_node{field_of_arg1=F1,
		       condition_number_of_arg2=Nth,
		       field_of_arg2=F2} = Test,
    lists:flatmap(fun(T) ->
			  [Wrl|Tt] = T,
			  {Keys,Gwrls} = group_wrs(Wrl,F1),
			  Gtrs = split_t(Tt,F2,Nth),
			  lists:foldr(fun(Key,Acc) ->
					      Sgt = proplists:get_all_values(Key,Gtrs),
					      if Sgt /= [] ->
						      Wrl2 = proplists:get_value(Key,Gwrls),
						      lists:map(fun(St) ->
									[Wrl2|St]
								 end,Sgt) ++ Acc;
						 true -> Acc
					      end
				      end,[],Keys)
		  end,Tl).
		      
		      
match([],Tl) ->
    Tl;
match([Test|T],Tl) ->
    TestTl = split_match(Test,Tl),
    match(T,TestTl).
		       
lperform_join_tests(Tests,Tl,Wrl) ->
    if Wrl == [] ->%orelse Tl == []->
	    [];
       true ->
	    NewTl = lists:map(fun(T)->
						%Token = runes_kb:get_token(Tr),
				      [Wrl|T]
			      end,Tl),
	    match(Tests,NewTl)
    end.

rperform_join_tests(Tests,Trl,Wrl) ->
    if Trl == []  orelse Wrl == []->
	    [];
       true ->
	    NewTl = lists:map(fun(Tr) ->
				      T = runes_kb:get_token(Tr),
				      [Wrl|T]
			      end,Trl),
	    match(Tests,NewTl)
    end.

merge({NewT,_N},[]) ->
    runes_kb:make_token_ref(NewT);
merge({NewT,N},[Otr|Left]) ->
    Token = runes_kb:get_token(Otr),
    {L1,[Wrl|L2]} = lists:split(N,Token),
    {N1,[Nw|N2]} = lists:split(N,NewT),
    if L1==N1 andalso L2==N2 ->
	    runes_kb:insert(token,Otr,L1++[(Nw++Wrl)|L2]),
	    yes;
       true ->
	    merge({NewT,N},Left)
    end.




