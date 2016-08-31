CQRS erlang
===========

** THIS CODE IS UNMANTAINED **

To use the gen_aggregate, implement the callbacks attempt command and apply event. The ideia is, 
you try to alter the process state using a command, and if the business rules inside command 
implementation agree, we apply the event to the state. Loading from repository, saving to 
the event sourcing database, and much more, should be done by the gen_aggregate process. 

For you comfort, the dev environment is setup on relx-dev.config, and use 'make run' to start 
developing with sync, observer, debuger and auto-reload. For production point the Makefile to relx.config. 

'make tests' to run all tests (unit and common tests) 
'make ct'    to run only system common tests

Sync makes the automatic reload and compile, and options is set on the 
'sync.config' file. Note that it includes the test folder. 

Note that in the cqrs-with-erlang [example](https://github.com/bryanhunter/cqrs-with-erlang/blob/ndc-oslo/bank/src/bank_command_handler.erl) 
the command_handler is saving the event in the event store after changing the 
process state. In scala actor persistence, they suggest to save, get an ack and 
afterwards change the state in memory. But the dilemma is, sometimes business 
rules can block the state changes, and the state was already saved. 
One possible solution is to process the state, and get ok from all the business 
rules, and "one line" before changing the memory state, save on disk, and in case 
we couldn't save we abort changing the state on memory. 

In the cqrs with Erlang examples, the 'process_event_changes' cache some events before persisting 
them to disk, it also poses a risk of failure, if process shutdowns before the info is written, 
so let's change this design also. 

For the sake of simplicity, let's use the native process registry, and later we can upgrade to 
gproc or syn : 
http://www.ostinelli.net/an-evaluation-of-erlang-global-process-registries-meet-syn/

### dependency map
aggregates, applications, workflows [uses] cqrs
aggregates, applications, workflows [implement behaviour] cqrs
cqrs [uses] repository
repository [uses] replay, app [starts] apps_sup, 
repository [uses] event_store, 
snapshooter [uses] event_store, 

Events
------

All events should have the #meta record, see the counter_test.hrl to see how to use it.


States
------

Also states should have #meta record to store meta-data. See counter_test.hrl.

Tools
-----

Use our redis-commander version, that can translate Erlang binaries using BERT: 
git@github.com:work-capital/redis-commander.git 
branch: bert-compatible


Aggregator Dependency
---------------------

http://jensrantil.github.io/cqrsevent-sourcing-messaging-patterns.html 



Directory Structure
-------------------

Every app will have it's own folder and main supervisor. The cqrs framework, will be still embbedded with 
specific applications, until we decide to open-source it as a library. So the cqrs_apps_sup.erl will start all 
application supervisors, and will be started by the cqrs_app. In case we need services running under the 
cqrs framework, cqrs_sup.erl will be free to start them. 

```
.
+-- accounts                        => account application
│   \-- bank_aggregate.erl
+-- counters                        => counter application
│   \-- counter_aggregate.erl
+-- cqrs_app.erl
+-- cqrs_bus.erl
+-- cqrs_command_handler.erl
+-- cqrs.erl
+-- cqrs_event_store.erl
+-- cqrs_factory.erl
+-- cqrs_repository.erl
+-- cqrs_apps_sup.erl               => connect cqrs with apps
\-- cqrs_sup.erl

```

Cqrs Flow
---------

Attempt command,
if successful, it creates an event,
  the event is recorded, 
  and applied.


Event Sourcing 
---------------

We can use three modalities:
  * pure event sourcing 
  * snapshot + event sourcing 
  * pure snapshot [so every state change we persist it] 



Repository
----------
We look for aggregators there. It uses the Factory if they are not available. 


Factory
-------
When aggregators are not available, factory will create them or restore them. 




Automatic Recovery
------------------
We can send command messages using the main command api even to dead aggregators, 
so repository will automatically rebuild them for you, simple as magic ! 


#### snapshot process

1. check if there is snapshots or events
2. if there is none of them, it's a new State, snapshot it and returns this RawState.
3. if there is Snapshot and no events, return the snapshot
4. if there is Snapshot and events, check if events are posterior of that snapshot
5. if events are anterior, return the snapshot (once this is the last konwn state)
6. if events are posterior, replay the state and events, and return the restored state



Configuration
-------------

For now, we use ?DEFINE, but after we move to a global dictionary using gproc, so all nodes will see them.

Redis
-----

### backups
  http://www.tutorialspoint.com/redis/redis_backup.htm 

