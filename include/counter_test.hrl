%%%===================================================================
%%% Counter [this is used only for the counter_aggregate tests in test folder
%%%===================================================================
-define(SNAPSHOT_PERIOD, 5).

-define(COUNTER_STATE_VER, "0.1.0").
-record(counter_state, {metadata = #meta{snapshot_period = ?SNAPSHOT_PERIOD, version = ?COUNTER_STATE_VER},
                       date_bumped, activation_date, counter_value, active}).

%%% EVENTS
-define(COUNTER_EVENT_VER, "0.1.0").
-record(counter_activated,  {metadata = #meta{snapshot_period = ?SNAPSHOT_PERIOD, version = ?COUNTER_EVENT_VER},
                             activation_date, 
                             initial_value}).

-record(owner_changed,      {metadata = #meta{snapshot_period = ?SNAPSHOT_PERIOD, version = ?COUNTER_EVENT_VER},
                             new_owner, 
                             date}).

-record(counter_bumped,     {metadata = #meta{snapshot_period = ?SNAPSHOT_PERIOD, version = ?COUNTER_EVENT_VER},
                             date_bumped}).
