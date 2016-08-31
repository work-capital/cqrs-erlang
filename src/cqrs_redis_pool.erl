-module(cqrs_redis_pool).
%%% see sys-dev.config or sys.config with episcina configs for redis. Pool will point to this module
%%% to get redis connections. http://blog.erlware.org/erlang-postgres-connection-pool-with-episcina/
%%% with pool 100.000 reads/6 seconds,  without pool, 100.000 reads/170 seconds

-define(REDIS_HOST, "127.0.0.1").
-define(REDIS_PORT, 6379).
-compile(export_all).


%%%===================================================================
%%% Specs
%%%===================================================================
-spec open(term(), term()) -> {ok, pid()}.    % needed by episcina
-spec close(pid()) -> ok.
-spec query(string()) -> tuple().
-spec get_connection(atom()) -> {redis_pool, pid()} | {error, timeout}.
-spec return_connection(atom(), {redis_pool, pid()}) -> ok.


%%%===================================================================
%%% API
%%%===================================================================

open(Host, Port) -> % {connection_error,{connection_error,econnrefused} TODO: check other possible errors
    {ok, Client} = eredis:start_link(?REDIS_HOST, ?REDIS_PORT),
    {ok, Client}.

close(Client) ->
  eredis:stop(Client).  %TODO: use a pool instead of opening and closing (so less exit messages around also)


%%%===================================================================
%%% Internal
%%%===================================================================

get_connection(Pool) ->
    case episcina:get_connection(Pool) of
        {ok, Pid} ->
            Pid;
        {error, timeout} ->
            {error, timeout}
    end.

return_connection(Pool, Pid) ->
    episcina:return_connection(Pool, Pid).


% imitate this code if you want to use the clients
query(Query) ->
    Client = get_connection(redis_pool),
    try
        eredis:q(Client, Query)
    after
        return_connection(redis_pool, Client)
    end.
