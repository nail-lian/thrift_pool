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
-module(thrift_pool_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    thrift_pool_sup:start_link().

stop(_State) ->
    ok.
