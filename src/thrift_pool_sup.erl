%%%-------------------------------------------------------------------
%%% @author Phillip Coleman <phil@datafiniti.net>
%%% @copyright (C) 2014, Datafiniti Inc.
%%% @doc This is a Thrift connection manager that was based off the work
%%%     done for the Eredis_pool by Hiroe Shin

%%% @author Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%%-------------------------------------------------------------------

-module(thrift_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/2]).
-export([create_pool/3, delete_pool/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {ok, Pools} = application:get_env(thrift_pool, pools),
    {ok, GlobalOrLocal} = application:get_env(thrift_pool, global_or_local),
    start_link(Pools, GlobalOrLocal).

start_link(Pools, GlobalOrLocal) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Pools, GlobalOrLocal]).

%% ===================================================================
%% @doc create new pool.
%% @end
%% ===================================================================
-spec(create_pool(PoolName::atom(), Size::integer(), Options::[tuple()]) ->
             {ok, pid()} | {error,{already_started, pid()}}).

create_pool(PoolName, Size, Options) ->
    PoolSpec = {PoolName, {poolboy, start_link, [[{name,{global, PoolName}},
                                                  {worker_module,thrift_recon_client},
                                                  {size, Size},
                                                  {max_overflow, 10}]
                                                 ++ Options
                                                ]},
                permanent, 5000, worker,
                [poolboy,thrift]},

    supervisor:start_child(?MODULE, PoolSpec).

%% ===================================================================
%% @doc delet pool and disconnected to Riak.
%% @end
%% ===================================================================
-spec(delete_pool(PoolName::atom()) -> ok | {error,not_found}).

delete_pool(PoolName) ->
    supervisor:terminate_child(?MODULE, PoolName),
    supervisor:delete_child(?MODULE, PoolName).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Pools, GlobalOrLocal]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    PoolSpecs = lists:map(fun({PoolName, PoolConfig, WorkerArgs}) ->
                                  Args = [{name, {GlobalOrLocal, PoolName}},
                                          {worker_module, thrift_recon_client}]
                                      ++ PoolConfig,
                                  poolboy:child_spec(PoolName, Args, WorkerArgs)
                          end, Pools),

    {ok, {SupFlags, PoolSpecs}}.
