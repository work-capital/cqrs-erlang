-module(complete_restore_tests).
-compile(export_all).

-define(REDIS_HOST, "127.0.0.1").
-define(REDIS_PORT, 6379).

-ifdef(TEST).
%-define(NODEBUG, true).  % turn on/off debug messages (should be before include eunit.hrl
-include_lib("eunit/include/eunit.hrl").
-include("cqrs.hrl").
-include("counter_test.hrl").

% METADATA [states, commands, events]
% -record(meta, {timestamp, uuid, version, user, counter }).
% % STATE
% -record(counter_state,     {metadata = #meta{}, pid_name, date_bumped, activation_date, counter_value, active}).
% % EVENTS
% -record(counter_activated, {metadata = #meta{}, activation_date, initial_value}).
% -record(owner_changed,     {metadata = #meta{}, new_owner, date}).
% -record(counter_bumped,    {metadata = #meta{}, date_bumped}).

complete_restore_test_() ->
    {spawn,
     [{setup,
       fun setup/0,
       fun cleanup/1,
       fun(Ref) ->
       [append_get_event_case(Ref),
        get_pid_name_case(Ref),
        get_many_events_case(Ref) ]
       end}]}.

setup() ->
    register (jom, self()),
    Id      = self(),
    State   = #counter_state{ metadata = meta(), counter_value = 0},    print("state", State),
    Evt1    = #counter_activated{ metadata = meta(), activation_date = t(), initial_value = 32},
    Evt2    = #counter_bumped{ metadata = meta(), date_bumped = t()},
    Events  = [Evt1, Evt2],
    Bumps   = [[] ++ Evt2 || X <- lists:seq(1,30)],    % populate with more 30 bumps
    print("events2", Bumps),
    {ok, Client} = eredis:start_link(?REDIS_HOST, ?REDIS_PORT),
    [Id, State, Events ++ Bumps, Client].

cleanup([Id, State, Events, Client]) ->
    ok.

append_get_event_case([Id, State, Events, Client])->
    fun() ->
      {registered_name, Pidname} = erlang:process_info(Id, registered_name),
      print("event list", Events),
      F = fun(State1, Event1, Client1, Pidname1, Key1) ->
            print("before", State1),
            State2 = counter_aggregate:apply_event(Event1, State1),
            print("after", State2),
            {ok, _, Index1} = cqrs_event_store:append_event(Event1, Client1, Pidname1, Key1),
            cqrs_event_store:maybe_make_snapshot(State2, Event1, Client1, Pidname1, Index1)
          end,
      %cqrs_replay:rebuild_state(cqrs_aggregate, Event, State)
      [F(State,E ,Client,Pidname, gen_uuid()) || E <- Events],
      %print("events", Events),
      ?assertEqual(ok,ok)
    end.

get_pid_name_case([Id, State, Events, Client]) ->
    fun() ->
      {registered_name, Pidname} = erlang:process_info(Id, registered_name),
%%      Pidname = cqrs_event_store:get_pid_name(State),
      ?assertEqual(jom, Pidname)
    end.

% set long timeout, as we test a loop of inserts here
get_many_events_case(Client) ->
  {timeout, 10000, fun() ->
      ?assertEqual(ok,ok)
  end}.

% Exists = cqrs_event_store:is_snapshot_exists(Id),

%%%===================================================================
%%% Internal functions
%%%===================================================================


meta() ->
  #meta{uuid       = gen_uuid(),
        version    = 1,
        id         = jeks,
        timestamp  = cqrs_util_date:get_timestamp()}.
t() ->
  erlang:localtime().

print(Name, Tuple) ->
    ToPrint = io_lib:format("Got ~p ==>  ~p",[Name, Tuple]),
    ?debugMsg(ToPrint).

gen_uuid() -> uuid:uuid_to_string(uuid:get_v4(), standard).
-endif.
%% 1. save snapshot
%% 2. save events
%% 3. load snapshot
%% 4. load events
%% 5. replay

