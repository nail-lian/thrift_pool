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
-module(thrift_pool).

%% Default timeout for calls to the client gen_server
%% Specified in http://www.erlang.org/doc/man/gen_server.html#call-3
-define(TIMEOUT, 10000).

%% API
-export([start/0, stop/0]).
-export([call/3, call/4, create_pool/2, create_pool/3, 
         create_pool/4, create_pool/5,create_pool/6, 
         create_pool/7, delete_pool/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

%% ===================================================================
%% @doc create new pool.
%% @end
%% ===================================================================
-spec(create_pool(PoolName::atom(), Size::integer()) -> 
             {ok, pid()} | {error,{already_started, pid()}}).

create_pool(PoolName, Size) ->
    thrift_pool_sup:create_pool(PoolName, Size, []).

-spec(create_pool(PoolName::atom(), Size::integer(), Host::string()) -> 
             {ok, pid()} | {error,{already_started, pid()}}).

create_pool(PoolName, Size, Host) ->
    thrift_pool_sup:create_pool(PoolName, Size, [{host, Host}]).

-spec(create_pool(PoolName::atom(), Size::integer(), 
                  Host::string(), Port::integer()) -> 
             {ok, pid()} | {error,{already_started, pid()}}).

create_pool(PoolName, Size, Host, Port) ->
    thrift_pool_sup:create_pool(PoolName, Size, [{host, Host}, {port, Port}]).

-spec(create_pool(PoolName::atom(), Size::integer(), 
                  Host::string(), Port::integer(), Database::string()) -> 
             {ok, pid()} | {error,{already_started, pid()}}).

create_pool(PoolName, Size, Host, Port, Database) ->
    thrift_pool_sup:create_pool(PoolName, Size, [{host, Host}, {port, Port},
                                                 {database, Database}]).

-spec(create_pool(PoolName::atom(), Size::integer(), 
                  Host::string(), Port::integer(), 
                  Database::string(), Password::string()) -> 
             {ok, pid()} | {error,{already_started, pid()}}).

create_pool(PoolName, Size, Host, Port, Database, Password) ->
    thrift_pool_sup:create_pool(PoolName, Size, [{host, Host}, {port, Port},
                                                 {database, Database},
                                                 {password, Password}]).

-spec(create_pool(PoolName::atom(), Size::integer(), 
                  Host::string(), Port::integer(), 
                  Database::string(), Password::string(),
                  ReconnectSleep::integer()) -> 
             {ok, pid()} | {error,{already_started, pid()}}).

create_pool(PoolName, Size, Host, Port, Database, Password, ReconnectSleep) ->
    thrift_pool_sup:create_pool(PoolName, Size, [{host, Host}, {port, Port},
                                                 {database, Database},
                                                 {password, Password},
                                                 {reconnect_sleep, ReconnectSleep}]).


%% ===================================================================
%% @doc delete pool and disconnect to Riak.
%% @end
%% ===================================================================
-spec(delete_pool(PoolName::atom()) -> ok | {error,not_found}).

delete_pool(PoolName) ->
    thrift_pool_sup:delete_pool(PoolName).

%%--------------------------------------------------------------------
%% @doc
%% The different options for the thrift call
%% @end
%%--------------------------------------------------------------------
-spec call(any(), atom(), list()) -> {any(), {ok, any()} | {error, any()}}.

call(PoolName, Function, Args)
  when is_atom(Function), is_list(Args) ->
    poolboy:transaction(PoolName, fun(Worker) ->
                                            thrift_reconnecting_client:call(Worker, Function, Args)
                                    end, ?TIMEOUT).

call(PoolName, Function, Args, Timeout)
  when is_atom(Function), is_list(Args), is_integer(Timeout) ->
    poolboy:transaction(PoolName, fun(Worker) ->
                                            thrift_reconnecting_client:call(Worker, Function, Args)
                                    end, Timeout).
