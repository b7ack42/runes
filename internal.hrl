-record(rule {name, lhs, rhs}).
-record(pattern {id, var, func, value}).


-record (fields, {id,
		  attr,
		  value}).

-record (wme, {fields,
	      alpha_mems,    %list
	      token_refs}).

-record (token, {parent,
		wme_ref,    %list
		node,
		children,
		join_results,
		owner}).



-record(alpha_memory, {%type,
		       wme_refs, 
		       succs,
		       ref_count = 0}).
-record(constant_test_node, {%type,
			     field,
			     value,
			     out_put_mem = [],
			     children = []}).
-record(rete_node, {type, %beta-m, join,p-node
		    children,
		    parent,
		    variant}).
-record(beta_memory, {token_refs,
		      all_children}).
-record(join, {amem,
	       tests,
	       nearest_ancestor_with_same_amem}).
-record(test_at_join_node, {field_of_arg1,
			    condition_number_of_arg2,
			    field_of_arg2}).
