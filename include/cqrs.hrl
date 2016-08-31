%%%===================================================================
%%% Metadata
%%%===================================================================


% METADATA [states, commands, events]  See counter_test.hrl to understand how to use it !
% This record defines the metadata used both by states, command and events.
% timestamp - The timestamp in UTC format of the incident.
% uuid      - This is a universal id of the incident (event, state or command).
%           - It's in the standard UUID v4 format.
% version   - The version of the current incident description.
% origin    - This field defines the origin of this incident. For example who initiated the incident.
% index     - This field is used for indexing the incident.
%             Index means the index of the event saved to the specific aggregator list.
% counter   - This field defines a continuous counter of this type of incident.
%           - (e.g.: Which serial number is the current event.
% type      - This is the type of the module that manages this events/states.
% id        - This field describes the target id. (e.g.: CNPJ).
% snapshot_period - Describes the number of events between snapshots.
%                 - If 0 is used in the event metadata, no events are kept.
%                   In this case it behaves like a simple state persistence system.
%                 - Be aware that if no events is kept it's a must to have 0 also in the
%                   metadata of the state to ensure that every state change is kept.
%                 - If 1 is used every event is kept and every event the state is updated.
-record(meta, {timestamp, uuid, version = "0.1.0", origin, index, type, id, snapshot_period = 10}).

% @TODO: When adding hot-code-swap, need to update the state version dynamically.
% @TODO: Add tests for 0 and 1 case of snapshot_period.
