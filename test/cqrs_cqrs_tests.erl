-module(cqrs_cqrs_tests).

-include("cqrs.hrl").
-include("counter_test.hrl").
-include_lib("eunit/include/eunit.hrl").

-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.


namespace_test() ->
  ?assertEqual(my_aggregate_jim, cqrs:add_namespace(jim, my_aggregate)),
  ?assertEqual(my_aggregate_jim, cqrs:add_namespace("jim",my_aggregate)).
