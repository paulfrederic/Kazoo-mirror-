%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ts_offnet_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/2]).

-export([listener/2
         ,build_endpoints/1
         ,call_event/2
        ]).

%% gen_fsm callbacks
-export([init/1

         ,build/2, build/3
         ,ringing/2, ringing/3
         ,answered/2, answered/3
         ,finished/2, finished/3

         ,handle_event/3
         ,handle_sync_event/4
         ,handle_info/3
         ,terminate/3
         ,code_change/4
        ]).

-include("ts.hrl").

-record(state, {endpoint :: api_object()
                ,failover :: api_object()
                ,other_leg :: api_binary()
                ,call :: whapps_call:call()
                ,call_id :: api_binary()
                ,listener :: api_pid()
                ,supervisor :: pid()
               }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Supervisor, Call) ->
    gen_fsm:start_link(?MODULE, [Supervisor, Call], []).

-spec listener(pid(), pid()) -> 'ok'.
listener(FSM, Listener) ->
    gen_fsm:send_event(FSM, {'listener', Listener}).

-spec build_endpoints(pid()) -> 'ok'.
build_endpoints(FSM) ->
    gen_fsm:send_event(FSM, 'build_endpoints').

-spec call_event(pid(), tuple()) -> 'ok'.
call_event(FSM, CallEvent) ->
    gen_fsm:send_event(FSM, CallEvent).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([Supervisor, Call]) ->
    whapps_call:put_callid(Call),
    {'ok', 'build', #state{call=Call
                           ,call_id=whapps_call:call_id(Call)
                           ,supervisor=Supervisor
                          }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
build({'listener', Listener}, State) ->
    lager:debug("recv listener pid: ~p", [Listener]),
    {'next_state', 'build', State#state{listener=Listener}};
build('build_endpoints', #state{call=Call
                                ,listener=Listener
                               }=State) ->
    ToDID = wnm_util:to_e164(whapps_call:to_user(Call)),

    lager:info("trying to build endpoint for ~s", [ToDID]),
    case build_endpoint(Call, ToDID) of
        {'ok', Endpoint, Failover} ->
            lager:debug("built endpoint: ~p", [Endpoint]),
            lager:debug("maybe build failover: ~p", [Failover]),
            ts_offnet_listener:bridge(Listener, Endpoint),
            {'next_state', 'ringing', State#state{failover=Failover}};
        {'error', 'no_endpoints'} ->
            lager:info("failed to find endpoints for ~s", [ToDID]),
            ts_offnet_listener:hangup(Listener),
            {'next_state', 'build', State}
    end;
build(_Evt, State) ->
    lager:debug("unhandled message in build/2: ~p", [_Evt]),
    {'next_state', 'build', State}.

build(_Evt, _From, State) ->
    lager:debug("unhandled message in build/3: ~p", [_Evt]),
    {'reply', {'error', 'unhandled'}, 'build', State}.

%% We are ringing an endpoint
ringing({'leg_created', OtherLeg}, State) ->
    lager:debug("other leg created: ~s", [OtherLeg]),
    {'next_state', 'ringing', State#state{other_leg=OtherLeg}};
ringing({'channel_bridged', OtherLeg}, #state{other_leg=OtherLeg}=State) ->
    lager:info("successfully bridged to ~s", [OtherLeg]),
    {'next_state', 'answered', State};
ringing({'leg_destroyed', OtherLeg, _Reason}, #state{other_leg=OtherLeg
                                                     ,failover='undefined'
                                                     ,listener=Listener
                                                    }=State) ->
    lager:info("failed to answer with no failover ~s: ~s", [OtherLeg, _Reason]),
    ts_offnet_listener:hangup(Listener),
    {'next_state', 'finished', State#state{other_leg='undefined'}};
ringing({'leg_destroyed', OtherLeg, _Reason}, #state{other_leg=OtherLeg
                                                     ,failover=Failover
                                                     ,listener=Listener
                                                    }=State) ->
    lager:info("failed to answer ~s: ~s", [OtherLeg, _Reason]),
    lager:info("trying failover route: ~p", [Failover]),
    ts_offnet_listener:bridge(Listener, Failover),
    {'next_state', 'ringing', State#state{other_leg='undefined'}};
ringing(_Evt, State) ->
    lager:debug("unhandled message in ringing/2: ~p", [_Evt]),
    {'next_state', 'ringing', State}.

ringing(_Evt, _From, State) ->
    lager:debug("unhandled message in ringing/3: ~p", [_Evt]),
    {'reply', {'error', 'unhandled'}, 'ringing', State}.

%% The endpoint has answered
answered({'leg_destroyed', OtherLeg, _Reason}, #state{other_leg=OtherLeg
                                                      ,listener=Listener
                                                     }=State) ->
    lager:debug("done after other leg ~s was destroyed: ~s", [OtherLeg, _Reason]),
    ts_offnet_listener:hangup(Listener),
    {'next_state', 'finished', State#state{other_leg='undeifned'}};
answered({'channel_destroy', CallId}, #state{call_id=CallId}=State) ->
    lager:debug("recv destroy for ~s", [CallId]),
    {'stop', 'normal', State};
answered(_Evt, State) ->
    lager:debug("unhandled message in answered/2: ~p", [_Evt]),
    {'next_state', 'answered', State}.

answered(_Evt, _From, State) ->
    lager:debug("unhandled message in answered/3: ~p", [_Evt]),
    {'reply', {'error', 'unhandled'}, 'answered', State}.

%% The call has finished
finished({'channel_destroy', CallId}, #state{call_id=CallId}=State) ->
    lager:debug("channel ~s destroyed", [CallId]),
    {'stop', 'normal', State};
finished(_Evt, State) ->
    lager:debug("unhandled message in finished/2: ~p", [_Evt]),
    {'next_state', 'finished', State}.

finished(_Evt, _From, State) ->
    lager:debug("unhandled message in finished/3: ~p", [_Evt]),
    {'reply', {'error', 'unhandled'}, 'finished', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    lager:debug("unhandled evt in ~s: ~p", [StateName, _Event]),
    {'next_state', StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    lager:debug("unhandled sync_evt in ~s: ~p", [StateName, _Event]),
    {'reply', {'error', 'unhandled'}, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    lager:debug("unhandled info in ~s: ~p", [StateName, _Info]),
    {'next_state', StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, #state{supervisor=Supervisor}) ->
    _ = spawn('ts_offnet_sup', 'stop', [Supervisor]),
    lager:debug("terminating while in state ~s: ~p", [_StateName, _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {'ok', StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
build_endpoint(_Call, ToDID) ->
    case wh_number_manager:lookup_account_by_number(ToDID) of
        {'ok', AccountId, _NumberProps} ->
            lager:debug("found number properties for ~s(~s)", [ToDID, AccountId]),
            RoutingData = routing_data(ToDID, AccountId),
            AuthUser = props:get_value(<<"To-User">>, RoutingData),
            AuthRealm = props:get_value(<<"To-Realm">>, RoutingData),

            InFormat = props:get_value(<<"Invite-Format">>, RoutingData, <<"username">>),
            Invite = ts_util:invite_format(wh_util:to_lower_binary(InFormat), ToDID) ++ RoutingData,

            {'ok'
             ,wh_json:from_list([{<<"Custom-Channel-Vars">>, wh_json:from_list([{<<"Auth-User">>, AuthUser}
                                                                                ,{<<"Auth-Realm">>, AuthRealm}
                                                                                ,{<<"Direction">>, <<"inbound">>}
                                                                                ,{<<"Account-ID">>, AccountId}
                                                                               ])
                                 }
                                 | Invite
                                ])
             ,props:get_value(<<"Failover">>, RoutingData)
            };
        {'error', _E} ->
            lager:debug("failed to find number properties for ~s: ~p", [ToDID, _E]),
            {'error', 'no_endpoints'}
    end.

-spec routing_data(ne_binary(), ne_binary()) -> wh_proplist().
-spec routing_data(ne_binary(), ne_binary(), wh_json:object()) -> wh_proplist().
routing_data(ToDID, AcctID) ->
    case ts_util:lookup_did(ToDID, AcctID) of
        {ok, Settings} ->
            lager:info("got settings for DID ~s", [ToDID]),
            routing_data(ToDID, AcctID, Settings);
        {error, no_did_found} ->
            lager:info("DID ~s not found in ~s", [ToDID, AcctID]),
            throw(no_did_found)
    end.

routing_data(ToDID, AcctID, Settings) ->
    AuthOpts = wh_json:get_value(<<"auth">>, Settings, wh_json:new()),
    Acct = wh_json:get_value(<<"account">>, Settings, wh_json:new()),
    DIDOptions = wh_json:get_value(<<"DID_Opts">>, Settings, wh_json:new()),
    RouteOpts = wh_json:get_value(<<"options">>, DIDOptions, []),

    NumConfig = case wh_number_manager:get_public_fields(ToDID, AcctID) of
                    {ok, Fields} -> Fields;
                    {error, _} -> wh_json:new()
                end,

    AuthU = wh_json:get_value(<<"auth_user">>, AuthOpts),
    AuthR = wh_json:get_value(<<"auth_realm">>, AuthOpts, wh_json:get_value(<<"auth_realm">>, Acct)),

    {Srv, AcctStuff} = try
                           {ok, AccountSettings} = ts_util:lookup_user_flags(AuthU, AuthR, AcctID),
                           lager:info("got account settings"),
                           {
                             wh_json:get_value(<<"server">>, AccountSettings, wh_json:new())
                             ,wh_json:get_value(<<"account">>, AccountSettings, wh_json:new())
                           }
                       catch
                           _A:_B ->
                               lager:info("failed to get account settings: ~p: ~p", [_A, _B]),
                               {wh_json:new(), wh_json:new()}
                       end,

    SrvOptions = wh_json:get_value(<<"options">>, Srv, wh_json:new()),

    case wh_util:is_true(wh_json:get_value(<<"enabled">>, SrvOptions)) of
        false -> throw({server_disabled, wh_json:get_value(<<"id">>, Srv)});
        true -> ok
    end,

    InboundFormat = wh_json:get_value(<<"inbound_format">>, SrvOptions, <<"npan">>),

    {CalleeName, CalleeNumber} = callee_id([
                                            wh_json:get_value(<<"caller_id">>, DIDOptions)
                                            ,wh_json:get_value(<<"callerid_account">>, Settings)
                                            ,wh_json:get_value(<<"callerid_server">>, Settings)
                                           ]),

    ProgressTimeout = ts_util:progress_timeout([
                                                wh_json:get_value(<<"progress_timeout">>, DIDOptions)
                                                ,wh_json:get_value(<<"progress_timeout">>, SrvOptions)
                                                ,wh_json:get_value(<<"progress_timeout">>, AcctStuff)
                                               ]),

    BypassMedia = ts_util:bypass_media([
                                        wh_json:get_value(<<"media_handling">>, DIDOptions)
                                        ,wh_json:get_value(<<"media_handling">>, SrvOptions)
                                        ,wh_json:get_value(<<"media_handling">>, AcctStuff)
                                       ]),

    FailoverLocations = [
                         wh_json:get_value(<<"failover">>, NumConfig)
                         ,wh_json:get_value(<<"failover">>, DIDOptions)
                         ,wh_json:get_value(<<"failover">>, SrvOptions)
                         ,wh_json:get_value(<<"failover">>, AcctStuff)
                        ],
    lager:info("looking for failover in ~p", [FailoverLocations]),

    Failover = ts_util:failover(FailoverLocations),
    lager:info("failover found: ~p", [Failover]),

    Delay = ts_util:delay([
                           wh_json:get_value(<<"delay">>, DIDOptions)
                           ,wh_json:get_value(<<"delay">>, SrvOptions)
                           ,wh_json:get_value(<<"delay">>, AcctStuff)
                          ]),

    SIPHeaders = ts_util:sip_headers([
                                      wh_json:get_value(<<"sip_headers">>, DIDOptions)
                                      ,wh_json:get_value(<<"sip_headers">>, SrvOptions)
                                      ,wh_json:get_value(<<"sip_headers">>, AcctStuff)
                                     ]),

    IgnoreEarlyMedia = ts_util:ignore_early_media([
                                                   wh_json:get_value(<<"ignore_early_media">>, DIDOptions)
                                                   ,wh_json:get_value(<<"ignore_early_media">>, SrvOptions)
                                                   ,wh_json:get_value(<<"ignore_early_media">>, AcctStuff)
                                                  ]),

    Timeout = ts_util:ep_timeout([
                                  wh_json:get_value(<<"timeout">>, DIDOptions)
                                  ,wh_json:get_value(<<"timeout">>, SrvOptions)
                                  ,wh_json:get_value(<<"timeout">>, AcctStuff)
                                 ]),

    %% Bridge Endpoint fields go here
    %% See http://wiki.2600hz.org/display/whistle/Dialplan+Actions#DialplanActions-Endpoint
    [KV || {_,V}=KV <- [ {<<"Invite-Format">>, InboundFormat}
                         ,{<<"Codecs">>, wh_json:get_value(<<"codecs">>, Srv)}
                         ,{<<"Bypass-Media">>, BypassMedia}
                         ,{<<"Endpoint-Progress-Timeout">>, ProgressTimeout}
                         ,{<<"Failover">>, Failover}
                         ,{<<"Endpoint-Delay">>, Delay}
                         ,{<<"SIP-Headers">>, SIPHeaders}
                         ,{<<"Ignore-Early-Media">>, IgnoreEarlyMedia}
                         ,{<<"Endpoint-Timeout">>, Timeout}
                         ,{<<"Callee-ID-Name">>, CalleeName}
                         ,{<<"Callee-ID-Number">>, CalleeNumber}
                         ,{<<"To-User">>, AuthU}
                         ,{<<"To-Realm">>, AuthR}
                         ,{<<"To-DID">>, ToDID}
                         ,{<<"Route-Options">>, RouteOpts}
                       ],
           V =/= undefined,
           V =/= <<>>
    ].

callee_id([]) -> {undefined, undefined};
callee_id([undefined | T]) -> callee_id(T);
callee_id([<<>> | T]) -> callee_id(T);
callee_id([JObj | T]) ->
    case wh_json:is_json_object(JObj) andalso (not wh_json:is_empty(JObj)) of
        true ->
            case {wh_json:get_value(<<"cid_name">>, JObj), wh_json:get_value(<<"cid_number">>, JObj)} of
                {undefined, undefined} ->
                    callee_id(T);
                CalleeID -> CalleeID
            end;
        false ->
            callee_id(T)
    end.
