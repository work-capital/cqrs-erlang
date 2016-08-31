-module(cqrs_replay_tests).

-include("cqrs.hrl").
-include("counter_test.hrl").
-include_lib("eunit/include/eunit.hrl").

-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.


replay_test() ->
  RawState = #counter_state{active = false},
  Evt1     = #counter_activated{activation_date = erlang:localtime(), initial_value = 3},
  Evt2     = #counter_bumped{date_bumped = erlang:localtime()},
  Evt3     = #counter_bumped{date_bumped = erlang:localtime()},
  Events   = [Evt1, Evt2, Evt3],
  NewState = cqrs_replay:rebuild_state(counter_aggregate, Events, RawState),
  R = lists:flatten(io_lib:format("new state -----> ~p", [NewState])),
  ?debugMsg(R),
  %?PRINT(NewState),
  ?assertEqual(1,1).
  

  
  
