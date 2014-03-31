%%
%% Licensed to the Apache Software Foundation (ASF) under one
%% or more contributor license agreements. See the NOTICE file
%% distributed with this work for additional information
%% regarding copyright ownership. The ASF licenses this file
%% to you under the Apache License, Version 2.0 (the
%% "License"); you may not use this file except in compliance
%% with the License. You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

-module(thrift_recon_client).

-behaviour(gen_server).

%% API
-export([ call/3 ]).

-export([ start_link/1 ]).

%% gen_server callbacks
-export([ init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3 ]).

-record( state, {  
                  host,
                  port,
                  thrift_svc,
                  thrift_opts,
                  reconn_min,
                  reconn_max,
                  reconn_time
                } ).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link( [Host, Port,
            ThriftSvc, ThriftOpts,
            ReconnMin, ReconnMax] ) ->
  gen_server:start_link( ?MODULE,
                         [ Host, Port,
                           ThriftSvc, ThriftOpts,
                           ReconnMin, ReconnMax ],
                         [] ).

call( Pid, Op, Args ) ->
  gen_server:call( Pid, { call, Op, Args }, infinity ).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Start the server.
%%--------------------------------------------------------------------
init( [ Host, Port, TSvc, TOpts, ReconnMin, ReconnMax ] ) ->
  process_flag( trap_exit, true ),
  
  State = #state{ host         = Host,
                  port         = Port,
                  thrift_svc   = TSvc,
                  thrift_opts  = TOpts,
                  reconn_min   = ReconnMin,
                  reconn_max   = ReconnMax
                },

  { ok, State }.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                                   {reply, Reply, State, Timeout} |
%%                                                   {noreply, State} |
%%                                                   {noreply, State, Timeout} |
%%                                                   {stop, Reason, Reply, State} |
%%                                                   {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call( { call, Op, Args },
             _From,
             State) ->

  {Result, S}  = send_call(Op, Args, State),
  { reply, Result, S }.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast( _Msg, State ) ->
  { noreply, State }.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info( _Info, State ) ->
  { noreply, State }.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate( _Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change( _OldVsn, State, _Extra ) ->
  { ok, State }.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
send_call(Op, Args, State = #state{
                             host        = Host,
                             port        = Port,
                             thrift_svc  = TSvc,
                             thrift_opts = TOpts
			    }) ->
  {ok, New_Client} = (catch thrift_client_util:new( Host, Port, TSvc, TOpts )),
  Result = ( catch thrift_client:call( New_Client, Op, Args) ),


  case Result of
    { C, { ok, Reply } } ->
      ( catch thrift_client:close( C ) ),
      { {ok, Reply }, State };
    { C, { E, _Msg } } when E == error; E == exception ->
      ( catch thrift_client:close( C ) ),
      send_call(Op, Args, State);
    _Other ->
      ( catch thrift_client:close( New_Client ) ),
      send_call(Op, Args, State)
  end.
