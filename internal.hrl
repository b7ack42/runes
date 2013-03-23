-module(records).
-export([]).

-record(rule {name, lhs, rhs}).
-record(cond {var, fun, value}).

-record(wme {