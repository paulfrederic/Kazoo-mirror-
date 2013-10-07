%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Handles onnet calls destined for offnet resources.
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ts_offnet).

-behaviour(gen_listener).

-export([start_link/1
         ,events/2
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("ts.hrl").

-record(state, {call :: whapps_call:call()
                ,endpoints = [] :: wh_json:objects()
                ,other_leg :: api_binary()
               }).
-type state() :: #state{}.

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, [{'self', []}]).
-define(RESPONDERS, [{{?MODULE, 'events'}
                     ,[{<<"*">>, <<"*">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link(whapps_call:call()) -> startlink_ret().
start_link(Call) ->
    Bindings = [{'call', [{'callid', whapps_call:call_id(Call)}
                          ,{'restrict_to', ['events', 'destroy_channel']}
                         ]}
                | ?BINDINGS
               ],
    gen_listener:start_link(?MODULE, [
                                      {'bindings', Bindings}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                      ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                      ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                      %%,{basic_qos, 1}                % only needed if prefetch controls
                                     ], [Call]).

events(JObj, Props) ->
    events(JObj, Props, wh_util:get_event_type(JObj)).

events(JObj, Props, {<<"call_event">>, <<"CHANNEL_DESTROY">>}) ->
    gen_listener:cast(props:get_value('server', Props)
                      ,{'channel_destroy', wh_json:get_value(<<"Call-ID">>, JObj)}
                     );
events(JObj, Props, {<<"call_event">>, <<"CHANNEL_BRIDGE">>}) ->
    gen_listener:cast(props:get_value('server', Props)
                      ,{'channel_bridged', wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj)}
                     );
events(JObj, Props, {<<"call_event">>, <<"LEG_DESTROYED">>}) ->
    gen_listener:cast(props:get_value('server', Props)
                      ,{'leg_destroyed', wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj)}
                     );
events(_JObj, _Props, _Evt) ->
    lager:debug("recv unhandled event: ~p", [_Evt]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init([whapps_call:call()]) -> {'ok', state()}.
init([Call]) ->
    whapps_call:put_callid(Call),
    {'ok', #state{call=Call}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast('build_endpoints', #state{call=Call}=State) ->
    ToDID = wnm_util:to_e164(whapps_call:to_user(Call)),

    lager:debug("finding number properties of ~s", [ToDID]),
    case build_endpoint(Call, ToDID) of
        {'ok', Endpoints} ->
            lager:debug("built endpoints: ~p", [Endpoints]),
            gen_listener:cast(self(), 'bridge_to_endpoints'),
            {'noreply', State#state{endpoints=Endpoints}};
        {'error', 'no_endpoints'} ->
            lager:debug("failed to find endpoints for ~s", [ToDID]),
            whapps_call_command:hangup(Call),
            {'stop', 'normal', State}
    end;

handle_cast('bridge_to_endpoints', #state{call=Call
                                          ,endpoints=Endpoints
                                         }=State) ->
    lager:debug("trying to bridge to endpoints"),
    whapps_call_command:bridge(Endpoints, Call),
    {'noreply', State};

%% Call Event handlers
handle_cast({'channel_destroy', CallId}, #state{other_leg=CallId
                                                ,call=Call
                                               }=State) ->
    lager:debug("recv channel destroy event for other leg ~s, done here", [CallId]),
    whapps_call_command:hangup(Call),
    {'noreply', State#state{other_leg='undefined'}};
handle_cast({'channel_destroy', CallId}, State) ->
    lager:debug("recv channel destroy event for ~s, done here", [CallId]),
    {'stop', 'normal', State};
handle_cast({'channel_bridged', OtherLeg}, State) ->
    lager:debug("bridged to leg ~s", [OtherLeg]),
    {'noreply', State#state{other_leg=OtherLeg}};
handle_cast({'leg_destroyed', OtherLeg}, #state{other_leg=OtherLeg
                                                ,call=Call
                                               }=State) ->
    lager:debug("other leg ~s destroyed", [OtherLeg]),
    whapps_call_command:hangup(Call),
    {'noreply', State#state{other_leg='undefined'}};

handle_cast({'wh_amqp_channel',{'new_channel',_IsNew}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'created_queue', QueueName}}, #state{call=Call}=State) ->
    {'noreply', State#state{call=whapps_call:set_controller_queue(QueueName, Call)}};
handle_cast({'gen_listener',{'is_consuming','true'}}, State) ->
    lager:debug("started consuming, build endpoints"),
    gen_listener:cast(self(), 'build_endpoints'),
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming','false'}}, State) ->
    lager:debug("stopped consuming, what do?"),
    {'noreply', State};

handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, _State) ->
    {'reply', []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

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

            {'ok', [wh_json:from_list([{<<"Custom-Channel-Vars">>, wh_json:from_list([{<<"Auth-User">>, AuthUser}
                                                                                     ,{<<"Auth-Realm">>, AuthRealm}
                                                                                     ,{<<"Direction">>, <<"inbound">>}
                                                                                     ,{<<"Account-ID">>, AccountId}
                                                                                    ])
                                      }
                                      | Invite
                                     ])]
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
