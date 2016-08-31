-module(cqrs_event_store_tests).

-compile(export_all).

-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

-ifdef(TEST).
%-define(NODEBUG, true).  % turn on/off debug messages (should be before include eunit.hrl
-include("cqrs.hrl").
-include("counter_test.hrl").
-include_lib("eunit/include/eunit.hrl").


cqrs_event_store_test_() ->
    {spawn,
     [{setup,
       fun setup/0,
       fun cleanup/1,
       fun(Ref) ->
       [
        counter_snapshot_algorithm_case(Ref)
       ]
       end}]
    }.
% https://code.google.com/archive/p/redis/issues/662#c4 
% http://stackoverflow.com/questions/16221563/whats-the-point-of-multiple-redis-databases 
% the keyspace should be multiple instances instead of multipledbs
setup() ->
    ok.

cleanup(Client) ->
    ok.

counter_snapshot_algorithm_case(Client) ->
  fun() ->
    Snapshots = [6,12,18,24,30,36],
    Period = 6,
    Res = [cqrs_event_store:mod(S,Period) || S <- lists:seq(1,40)],
   %TODO:finish this test 
    print(Res),
    ?assertEqual(ok, ok)
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================



print(Tuple) ->
    ToPrint = io_lib:format("Got ==>  ~p",[Tuple]),
    ?debugMsg(ToPrint).

-endif.
