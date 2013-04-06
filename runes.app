{application, runes,
 [{description, "A simple rule engine"},
   {vsn, "0.1.0"},
   {modules, [
              runes_app,
   	      runes_sup,
	      runes_engine,
	      runes_compile,
	      runes_kb,
	      runes
	      ]},
   {registered,[runes_sup]},
   {application,[kernel,stdlib]},
   {mod, {runes_app,[]}}
   ]}.