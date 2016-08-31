-module(counter_aggregate_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

suite() -> 
    [{timetrap,{minutes,1}}]. 

%%====================================================================
%% Initializations 
%%====================================================================

init_per_suite(Config) -> 
    application:start(gproc),
    application:start(eredis),
    application:start(episcina),
    episcina:start_pools([{redis_pool,
                    [{size, 10},
                     {timeout, 100000},
                     {connect_provider, {cqrs_redis_pool, open, ["localhost", 6379]}},
                     {close_provider, {cqrs_redis_pool, close, []}} ]}]),
    application:start(cqrs),
    ct:pal(info, "EPNA supervisor state ~p detected! Info: ~p", [sys:get_state(epna_sup), "epna_sup"]),
    TableName = some_atom_to_test_it, 
    [{con_ref, example },{table_name, TableName}| Config]. 

end_per_suite(Config) ->    
    Ref = ?config(con_ref, Config),
    ct:pal(info, "Process ~p detected! Info: ~p", [Ref, "ent_per_suite"]),
    %eredis:stop(Ref),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Tests decalrations
%%====================================================================

groups() -> [{counter_group, [], 
             [start_case, register_case]}].

all() -> [test1, my_test_case, {group, counter_group}].


%%%===================================================================
%%% Group tests
%%%===================================================================


start_case(Config) ->
  1 = 1.

register_case(Config) ->
  1 = 1.

%%====================================================================
%% Isolated tests
%%====================================================================

test1(_Config) ->
  %counter_sup:start_counter_aggregate(i4),
  {ok, Pid} = counter_aggregate:start_link(counter1),
  {ok, Pid2} = counter_aggregate:start_link(counter2),

  %{ok, ProcessName2, Pid2} = counter_aggreate:start_link(jon),
  1 = 1.
 

my_test_case() -> 
    [].

my_test_case(_Config) -> 
    ok.



