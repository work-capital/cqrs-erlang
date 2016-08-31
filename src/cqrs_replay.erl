-module(cqrs_replay).
-compile(export_all).

-include_lib("../include/cqrs.hrl").

% all states should have version, id, owner, roles and creation_date
-record(state, {}).


%%%===================================================================
%%% API functions
%%%===================================================================

% all we need, who is applying events [module], a list of events and a raw state
rebuild_state(Module, Events, State) ->
  RequesterPid = self(),
  spawn(fun() -> apply_many_events(RequesterPid, Events, State, Module) end),
  receive
      {state, NewState} -> NewState
  end.


%%%===================================================================
%%% Internals
%%%===================================================================


apply_many_events(Parent, [], State, Module) ->
  Parent ! {state, State};
apply_many_events(Parent, [Event|Rest], State, Module) ->
  NewState = Module:apply_event(Event, State),
  apply_many_events(Parent, Rest, NewState, Module).





%%% TODO: some timeout on the start maybe will be good
%%% Create new aggregators or workflows from scratch, by snapshot or by replaying their events.
%%% Using a new process only to replay and rebuild the state, we achieve the necessary abstraction
%%% to use gen_servers for aggregators, without implementing apply_many_events in all of them,
%%% and also optimize, once the replays could be redicrected to a special node for it.
%%% to restore mass processes http://stackoverflow.com/questions/20339937/getting-result-of-a-spawned-function-in-erlang
%%% This won’t work: because the anonymous function only gets executed once the new process is started, the value returned by self() will be that process’s ID, and not that of the parent.
