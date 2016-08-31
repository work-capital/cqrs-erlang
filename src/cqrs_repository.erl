-module(cqrs_repository).
-compile(export_all).
-include("cqrs.hrl").

-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

%%%===================================================================
%%% API
%%%===================================================================

% if finds the snapshot, replay events from the last applied event on it
maybe_restore(NamespaceId, Id, Module, State, Origin) ->
  NewState = cqrs_event_store:update_meta(State, {Origin, Module, Id, undefined}),
  A = cqrs_event_store:load_snapshot(NamespaceId),
  case A of
    snapshot_not_found ->
      replay_from_events(NamespaceId, Module, NewState, 0);
    Snapshot ->
      SnapshotVersionUpdated = cqrs_event_store:copy_required_fields(Snapshot, NewState),
      Counter   = cqrs_event_store:get_index(SnapshotVersionUpdated),
      replay_from_events(NamespaceId, Module, SnapshotVersionUpdated, Counter)
  end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

replay_from_events(Id, Module, State, Counter) ->
  case cqrs_event_store:get_events(Id, Counter) of
    not_found ->
      {ok, State};
    Events ->
      NewState    = cqrs_replay:rebuild_state(Module, Events, State),     ?PRINT(NewState),
      {ok, NewState}
  end.



%%% RESPONSABILITIES
%%% 1. try to get a process from repository
%%% 2. if found, return it, if not, replay from source
%%% 3. facade API for aggregators with the event store

% 1. check if there is snapshots or events
% 2. if there is none of them, it's a new State, snapshot it and returns this RawState.
% 3. if there is Snapshot and no events, return the snapshot
% 4. if there is Snapshot and events, check if events are posterior of that snapshot
% 5. if events are anterior, return the snapshot (once this is the last konwn state)
% 6. if events are posterior, replay the state and events, and return the restored state


% save(Pid, Aggregator) ->  % has to know the aggregator to reach the right bucket. 
% 	Saver = fun(PidName, Events) -> cqrs_event_store:append_events(PidName, Events) end,
% 	Aggregator:process_unsaved_changes(Pid, Saver).

