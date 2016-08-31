-module(cqrs_event_store).
-compile(export_all).
-include("cqrs.hrl").
-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.
% TODO: move db config to file configs
-define(REDIS_HOST, "127.0.0.1").
-define(REDIS_PORT, 6379).
% record parse data abstraction
-define(META_POSITION,     2).

%%%===================================================================
%%% API functions
%%%===================================================================
% TODO: refactor to 3 functions, process_event, append_event, make_snapshot

process_event(State, Event, Origin) ->
  process_event(get_module_name (self()), State, Event, Origin).

process_event(Module, State, Event, Origin) ->

  Client = cqrs_redis_pool:get_connection(redis_pool),
  try
    Pid                   = self(),
    NewState              = Module:apply_event(Event, State),
    {registered_name, NamespaceId} = erlang:process_info(Pid, registered_name),
    Id                    = get_id (NamespaceId, Module),
    LinkKey               = gen_uuid(),
    EventMetaUpdated      = update_meta        (Event,    {Origin, Module, Id, undefined}),
    {ok, EventWithIndex, Index}  = append_event(EventMetaUpdated, Client, NamespaceId, LinkKey),
    StateMetaUpdated      = update_meta        (NewState, {Origin, Module, Id, Index}),
    ok = ensure_that_snapshot_period_are_equals(StateMetaUpdated, EventWithIndex),
    {ok, _}               = maybe_make_snapshot(StateMetaUpdated, EventWithIndex, Client, NamespaceId, Index),
    {ok, StateMetaUpdated, EventWithIndex, LinkKey, Index}
  after
    cqrs_redis_pool:return_connection(redis_pool, Client)
  end.


append_event(Event, Client, Id, Key) ->
  Meta = element(?META_POSITION, Event),
  case Meta#meta.snapshot_period of
    0 -> {ok, Event, 0};
    _ -> {ok, Size }           = eredis:q(Client, ["LLEN", Id]),          % we also enumerate events
         {ok, <<"OK">>}        = eredis:q(Client, ["MULTI"]),
         {ok, <<"QUEUED">>}    = eredis:q(Client, ["RPUSH", Id, Key]),
         EventWithIndex        = update_meta(Event, index, to_int(Size)),
         {ok, <<"QUEUED">>}    = eredis:q(Client, ["HSET", eventlog, Key, EventWithIndex]),    %
         {ok, [Last, <<"1">>]} = eredis:q(Client, ["EXEC"]), % when we exec, we receive back all returns from queues
         Index                 = decrease(Last),             % to sync with the key list, that initiates with 0
         {ok, EventWithIndex, Index}
  end.

get_event(Key)->
  {ok, Res} = cqrs_redis_pool:query(["HGET", eventlog, Key]),
  binary_to_term(Res).

% get a bunch of events to replay in an aggregator, first get a list of keys
% -spec get_events(term(), term(), atom, integer) -> list.
get_events(Id, InitialPosition) ->
  case cqrs_redis_pool:query(["LRANGE", Id, InitialPosition + 1, -1]) of   % -1 is = last element
    {ok,[]} ->
      not_found;
    {ok, KeyList}  ->
      Query         = ["HMGET", eventlog] ++ KeyList,
      {ok, Events}  = cqrs_redis_pool:query(Query),
      events_to_list(Events, []);
    {error, Reason} ->
      {error, Reason}
  end.

%returns -> snapshot_not_found | <<131,23,32,23,23, etc..>>
load_snapshot(Id) ->
  case cqrs_redis_pool:query(["HGET", snapshots, Id]) of
    {ok,undefined} -> snapshot_not_found;
    {ok,Result}    -> binary_to_term(Result)
  end.

is_snapshot_exists(ProcessName) ->
  case cqrs_redis_pool:query(["HEXISTS", snapshots, ProcessName]) of
    {ok, <<"0">>} -> false;
    {ok, <<"1">>} -> true
  end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================
ensure_that_snapshot_period_are_equals(State, Event) ->
  MetaState = element(?META_POSITION, State),
  MetaEvent = element(?META_POSITION, Event),
  S = MetaState#meta.snapshot_period,
  E = MetaEvent#meta.snapshot_period,
  S = E,
  ok.

maybe_make_snapshot(State, Event, Client, Id, Index) ->
  Meta           = element(?META_POSITION, State),
  case mod(Index, Meta#meta.snapshot_period) of   % {ok,<<"1">>} = new, <<"0">> = update, use Index as index for multi-snapshot.
    true  -> {ok, Result} = eredis:q(Client, ["HSET", snapshots, Id, update_meta(State, index, Index)]); % same key for snapsh.
    false -> {ok, postponed}
  end.

% C = Index, P = period, we calculate how many events we should snapshot.
mod(0,P) -> true;                      % we snapshot the state from the first event
mod(C,P) when C  < P -> false;         %TODO: make unit test
mod(C,P) when C >= P ->
  case C rem P of
    0 -> true;
    _ -> false
  end.

to_int(Binary) ->
  erlang:binary_to_integer(Binary).

decrease(Binary) ->                    % once event counting start with 0, we need to decrease the snapshot Index to sync it
  erlang:binary_to_integer(Binary) - 1.

concat(Atom, Binary) ->
  erlang:atom_to_list(Atom) ++ "-" ++erlang:integer_to_list(decrease(Binary)).

update_meta(EventOrState, {Origin, Module, Id, Index}) ->
  EventOrState1 = update_meta(EventOrState, timestamp, cqrs_util_date:get_timestamp()),
  EventOrState2 = update_meta(EventOrState1, uuid, gen_uuid()),
  EventOrState3 = update_meta(EventOrState2, origin, Origin),
  EventOrState4 = update_meta(EventOrState3, type, Module),
  EventOrState5 = update_meta(EventOrState4, id,   Id),
  update_meta(EventOrState5, index, Index).

update_meta(EventOrState, Field, Value) ->
  Meta     = element(?META_POSITION, EventOrState),
  Updated  = meta_set(Field, Meta, Value),
  setelement(?META_POSITION, EventOrState, Updated).

copy_required_fields(ToState, FromState) ->
  FromMeta = element(?META_POSITION, FromState),
  ToState1 = update_meta(ToState, version, FromMeta#meta.version),
  update_meta(ToState1, snapshot_period, FromMeta#meta.snapshot_period).

get_index(State) ->
  Meta     = element(?META_POSITION, State),
  ?PRINT(Meta),
  meta_get(index, Meta).

events_to_list([H|T], Acc) ->
  events_to_list(T, Acc ++ [binary_to_term(H)]);
events_to_list([], Acc) ->
  Acc.

gen_uuid() -> uuid:uuid_to_string(uuid:get_v4(), standard).

get_module_name (Pid) ->
  Dict        = erlang:process_info(Pid, dictionary),
  List        = element(2, Dict),
  InitialCall = lists:keyfind('$initial_call', 1, List),
  ModuleName  = element(1,element(2, InitialCall)),
  ModuleName.

get_id (NamespaceId, Namespace) ->
  NamespaceLength   = length(atom_to_list(Namespace)),
  list_to_atom(lists:nthtail(NamespaceLength + 1, atom_to_list(NamespaceId))).

%% ====================================================================
%% Used to find index of dynamic meta fields and update meta record
%% ====================================================================
meta_get(Field, Record)        -> element   (meta_field_index(Field), Record).
meta_set(Field, Record, Value) -> setelement(meta_field_index(Field), Record, Value).
meta_field_index(Field)        -> meta_index(Field, record_info(fields, meta), 2).
meta_index(M, [M|_], I)        -> I;
meta_index(M, [_|T], I)        -> meta_index(M, T, I+1).
%% ====================================================================

%%% RESTORE COMPLETE PROCESS
%%%
%%% 1. check if there is snapshots or events
%%% 2. if there is none of them, it's a new State, snapshot it and returns this RawState.
%%% 3. if there is Snapshot and no events, return the snapshot
%%% 4. if there is Snapshot and events, check if events are posterior of that snapshot
%%% 5. if events are anterior, return the snapshot (once this is the last konwn state)
%%% 6  if events are posterior, replay the state and events, and return the restored state

%%% to start, let's open and close connections, it gives us 300 writes/second.
%%% to scale, we can use poolboy https://github.com/hiroeorz/eredis_pool  or
%%% https://github.com/seth/pooler
%%% http://stackoverflow.com/questions/12895448/how-can-i-make-webmachine-and-eredis-work-together
%%% https://github.com/hiroeorz/eredis_pool -> use also pipeline TODO: add connection pool
