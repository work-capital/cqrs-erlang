-module(cqrs).
-include("cqrs.hrl").
-compile(export_all).


%TODO: write instructions on how to use this API facade

-callback apply_event(Event :: term(), State :: term()) ->
  error | term().

% TODO: instead of 'error', maybe a maybe ? erlando...
% the gen_server and gen_fsm handle commands nativelly
% -callback handle_command(Command :: term(), State :: term()) ->
%       {event, Event :: term()} |
%           {stop, Reason :: term()}.
%
% TODO: add gen_fsm calls 



%%%===================================================================
%%% API  [system facade]
%%%===================================================================


apply_event(State, Event, Initiator) ->
  cqrs_event_store:process_event(State, Event, Initiator).

maybe_restore (Id, Module, State, Origin) ->
  NamespaceId = cqrs:add_namespace(Id, Module),
  cqrs_repository:maybe_restore(NamespaceId, Id,  Module, State, Origin).

call (Pid, Module, Data) ->
  gen_server:call(cqrs:add_namespace(Pid, Module), Data).

cast (Pid, Module, Data) ->
  gen_server:cast(cqrs:add_namespace(Pid, Module), Data).

start_link (Id, Module) ->
  NamespaceId = cqrs:add_namespace(Id, Module),
  gen_server:start_link({local, NamespaceId}, Module, [Id], []).

start_link (Id, Module, DebugOptions) ->
  NamespaceId = cqrs:add_namespace(Id, Module),
  gen_server:start_link({local, NamespaceId}, Module, [Id], DebugOptions).

start_link (Id, Module, Args, DebugOptions) -> % if you want to send more arguments than Id
  NamespaceId = cqrs:add_namespace(Id, Module),
  gen_server:start_link({local, NamespaceId}, Module, [Id, Args], DebugOptions).

query(Query) ->                   % in case you have your own data implementation, and wants to go ACTIVE RECORD instead of CQRS
  cqrs_redis_pool:query(Query).   % anyway, take care with the namespaces if you use a hibrid solution.


%%%===================================================================
%%% Internal
%%%===================================================================

% add namespace using the Module name to the Id
add_namespace(Id, Module) when is_atom(Id) ->
  list_to_atom(atom_to_list(Module) ++ "_" ++ atom_to_list(Id));
add_namespace(Id, Module) when is_list(Id) ->
  list_to_atom(atom_to_list(Module) ++ "_" ++ Id).

