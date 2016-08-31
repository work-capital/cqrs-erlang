%%%-------------------------------------------------------------------
%%% This file is used just for testing the event_store_SUITE.
%%% Created : 29. Mar 2016 17:14
%%%-------------------------------------------------------------------
-module(car_aggregate).
-author("kalmus").

%% API
-compile(export_all).

-behaviour(cqrs).


%%%===================================================================
%%% Cqrs Callbacks
%%%===================================================================

apply_event({bought_car_event, MetaData, Type, Size, Model, New}, State) ->
  State;

apply_event({sold_car_event, MetaData, Type, Size, Model, New}, State) ->
  State.

