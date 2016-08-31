-module(counter_aggregate).
-compile(export_all).
%%% see ERLANG_REFERENCE.md file to see an example on how to build a supervisor for this gen_server
-include("cqrs.hrl").
-include("counter_test.hrl").
-behaviour(gen_server).
-behaviour(cqrs).

-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

%%%===================================================================
%%% API functions
%%%===================================================================
stop(Pid)                   -> cqrs:cast(Pid, ?MODULE, {stop, "enough for today"}).

start_link(Id)              -> cqrs:start_link(Id, ?MODULE).

bump(Pid)                   -> cqrs:cast(Pid, ?MODULE, {bump}).

change_owner(Pid, Owner)    -> cqrs:call(Pid, ?MODULE, {name, Owner}).

activate(Pid, InitialValue) -> cqrs:call(Pid, ?MODULE, {activate, InitialValue}).    % demo! use calls only!

%%%===================================================================
%%% Gen_server Callbacks
%%%===================================================================
init([Id]) ->
  State = #counter_state{},                     % MUST set Id !!!
  cqrs:maybe_restore(Id, ?MODULE, State, undefined).    % if it has a state to restore, the restored state will be back here

handle_call({activate, InitialValue}, _From, State) ->
  Event            = #counter_activated{initial_value = InitialValue},
  {ok, NewState, NewEvent, _, _} = cqrs:apply_event(State, Event, undefined),
  {reply, {NewState, NewEvent}, NewState}.

handle_cast({bump}, State) ->
  ?PRINT(State#counter_state.counter_value),
  Event            = #counter_bumped{date_bumped = erlang:localtime()},
  {ok, NewState, NewEvent, _, _} = cqrs:apply_event(State, Event, undefined),
  {noreply, NewState};

handle_cast({stop, Message}, State) ->
  {stop, normal, State}.

handle_info(_Info, State)           -> {noreply, State}.
terminate  (_Reason, _State)        ->  ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Cqrs Callbacks
%%%===================================================================

apply_event({counter_activated, MetaData, Activation_Date, InitialValue}, State) ->
  State#counter_state{activation_date = erlang:localtime(),
    counter_value   = InitialValue,
    active          = true};

apply_event({counter_bumped, MetaData, DateBumped}, State) ->
  NewCounterValue = State#counter_state.counter_value + 1,
  State#counter_state{counter_value   = NewCounterValue,
    date_bumped     = DateBumped}.
