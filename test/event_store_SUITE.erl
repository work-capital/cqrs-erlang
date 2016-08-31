-module(event_store_SUITE).
-compile(export_all).

-include("cqrs.hrl").
-include("counter_test.hrl").

-record(bought_car_event, {metadata = #meta{}, type, size, model, new}).
-record(sold_car_event, {metadata = #meta{}, type, size, model, new}).
-record(car_state, {metadata = #meta{} }).

-include_lib("common_test/include/ct.hrl").
suite() -> 
    [{timetrap,{minutes,1}}]. 

%%====================================================================
%% Initializations 
%%====================================================================

init_per_suite(Config) -> 
    application:start(cqrs),
    application:start(gproc),
    application:start(eredis),
    application:start(episcina),
    episcina:start_pools([{redis_pool,
                    [{size, 10},
                     {timeout, 100000},
                     {connect_provider, {cqrs_redis_pool, open, ["localhost", 6379]}},
                     {close_provider, {cqrs_redis_pool, close, []}} ]}]),
    ct:pal(info, "EPNA supervisor state ~p detected! Info: ~p", [sys:get_state(epna_sup), "epna_sup"]),
    Evt1 = #bought_car_event{type = "toyota", size = "big",   model = "corolla", new = true},
    Evt2 = #bought_car_event{type = "honda",  size = "big",   model = "civic",   new = false},
    Evt3 = #bought_car_event{type = "kia",    size = "small", model = "picanto", new = true},
    Events = [Evt1, Evt2, Evt3],
    ct:pal(info, "Events ~p detected! Info: ~p", [Events, "events"]),
    [{events, Events}]. 

end_per_suite(Config) ->    
    Ref = ?config(con_ref, Config),
    ct:pal(info, "Process ~p detected! Info: ~p", [Ref, "ent_per_suite"]),
    %eredis:stop(Ref),
    ok.

init_per_group(_GroupName, Config)   -> Config.
end_per_group(_GroupName, _Config)   -> ok.
init_per_testcase(_TestCase, Config) -> Config.
end_per_testcase(_TestCase, _Config) -> ok.

%%====================================================================
%% Tests decalrations
%%====================================================================

groups() -> [{append_group, [], 
             [append_case, many_events_case]}].

all() -> [test1, my_test_case, {group, append_group}].


%%%===================================================================
%%% Group tests
%%%===================================================================


append_case(Events) ->
    register(car_aggregate_jim, self()),
    State1 = #car_state{metadata = #meta{}},
    State2 = #car_state{metadata = #meta{}},
    State3 = #car_state{metadata = #meta{}},
    Evt1 = #bought_car_event{metadata = #meta{index=0},type = "kia",    size = "small", model = "picanto", new = true},
    Evt2 = #bought_car_event{metadata = #meta{index=1},type = "honda",  size = "big",   model = "civic",   new = false},
    Evt3 = #bought_car_event{metadata = #meta{index=2},type = "kia",    size = "small", model = "picanto", new = true},
    {ok, _, _, Key1, Index1} = cqrs_event_store:process_event(car_aggregate, State1, Evt1, undefined),
    {ok, _, _, Key2, Index2} = cqrs_event_store:process_event(car_aggregate, State2, Evt2, undefined),
    {ok, _, _, Key3, Index3} = cqrs_event_store:process_event(car_aggregate, State3, Evt3, undefined),
    Res1 = cqrs_event_store:get_event(Key1),
    Res2 = cqrs_event_store:get_event(Key2),
    Res3 = cqrs_event_store:get_event(Key3).


many_events_case(Config) ->
      register(car_aggregate_jum, self()),
      State1 = #car_state{metadata = #meta{}},
      State2 = #car_state{metadata = #meta{}},
      State3 = #car_state{metadata = #meta{}},
      Evt1 = #bought_car_event{metadata = #meta{index=0}, type = "mercedes", size = "S", model = "picanto", new = true},
      Evt2 = #bought_car_event{metadata = #meta{index=1}, type = "fiat",     size = "B", model = "civic",   new = false},
      Evt3 = #bought_car_event{metadata = #meta{index=2}, type = "jaguar",   size = "M", model = "picanto", new = true},
      EvtL = [Evt1, Evt2, Evt3],
      Ids    = [mca_jam, mca_jum, mca_jom, mca_jim],
      %[cqrs_event_store:process_event(car_aggregate, I, E) || E <- EvtL, V <- lists:seq(1, 10), I <- Ids],
      {ok, _, _,  Key1, Index1} = cqrs_event_store:process_event(car_aggregate, State1, Evt1, undefined),
      {ok, _, _,  Key2, Index2} = cqrs_event_store:process_event(car_aggregate, State2, Evt2, undefined),
      {ok, _, _,  Key3, Index3} = cqrs_event_store:process_event(car_aggregate, State3, Evt3, undefined),
      {ok, _, _,  Key4, Index4} = cqrs_event_store:process_event(car_aggregate, State2, Evt1, undefined),
      % ca_jam received 2 events from index1
      Events = cqrs_event_store:get_events("test1", Index1),
      ct:pal(info, "Events ~p detected! Info: ~p", [Events, "events"]).

%%====================================================================
%% Isolated tests
%%====================================================================

test1(_Config) ->
  A = 0,
  1/1.

my_test_case() -> 
    [].

my_test_case(_Config) -> 
    ok.



