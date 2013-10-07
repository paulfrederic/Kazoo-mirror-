%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Handles onnet calls destined for offnet resources.
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ts_offnet_listener).

-behaviour(gen_listener).

-export([start_link/2
         ,events/2

         ,bridge/2
         ,hangup/1
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
                ,supervisor :: pid()
                ,fsm :: pid()
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
-spec start_link(pid(), whapps_call:call()) -> startlink_ret().
start_link(Supervisor, Call) ->
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
                                     ], [Supervisor, Call]).

events(JObj, Props) ->
    events(JObj, Props, wh_util:get_event_type(JObj)).

events(JObj, Props, {<<"call_event">>, <<"CHANNEL_DESTROY">>}) ->
    Evt = {'channel_destroy', wh_json:get_value(<<"Call-ID">>, JObj)},
    gen_listener:cast(props:get_value('server', Props), Evt),
    ts_offnet_fsm:call_event(props:get_value('fsm', Props), Evt);
events(JObj, Props, {<<"call_event">>, <<"CHANNEL_BRIDGE">>}) ->
    Evt = {'channel_bridged', wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj)},
    ts_offnet_fsm:call_event(props:get_value('fsm', Props), Evt);
events(JObj, Props, {<<"call_event">>, <<"LEG_CREATED">>}) ->
    Evt = {'leg_created', wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj)},
    ts_offnet_fsm:call_event(props:get_value('fsm', Props), Evt);
events(JObj, Props, {<<"call_event">>, <<"LEG_DESTROYED">>}) ->
    Evt = {'leg_destroyed'
           ,wh_json:get_value(<<"Other-Leg-Unique-ID">>, JObj)
           ,wh_json:get_value(<<"Hangup-Cause">>, JObj)
          },
    ts_offnet_fsm:call_event(props:get_value('fsm', Props), Evt);
events(_JObj, _Props, _Evt) ->
    lager:debug("recv unhandled event: ~p", [_Evt]).

bridge(Listener, Endpoint) ->
    gen_listener:cast(Listener, {'bridge_to_endpoint', Endpoint}).

hangup(Listener) ->
    gen_listener:cast(Listener, 'hangup').

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
init([Supervisor, Call]) ->
    whapps_call:put_callid(Call),
    {'ok', #state{call=Call
                  ,supervisor=Supervisor
                 }}.

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
handle_cast({'bridge_to_endpoint', Endpoint}, #state{call=Call}=State) ->
    lager:info("trying to bridge to endpoints"),
    whapps_call_command:bridge([Endpoint], Call),
    {'noreply', State};
handle_cast('hangup', #state{call=Call}=State) ->
    lager:info("sending hangup to call"),
    whapps_call_command:hangup(Call),
    {'noreply', State};

%% Call Event handlers
handle_cast({'channel_destroy', _CallId}, #state{call=Call}=State) ->
    lager:debug("recv channel destroy event, done here"),
    whapps_call_command:hangup(Call),

    {'stop', 'normal', State};

handle_cast({'fsm', FSM}, State) ->
    lager:debug("setting fsm pid: ~p", [FSM]),
    ts_offnet_fsm:listener(FSM, self()),
    ts_offnet_fsm:build_endpoints(FSM),
    {'noreply', State#state{fsm=FSM}};
handle_cast({'wh_amqp_channel',{'new_channel',_IsNew}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'created_queue', QueueName}}, #state{call=Call}=State) ->
    {'noreply', State#state{call=whapps_call:set_controller_queue(QueueName, Call)}};
handle_cast({'gen_listener',{'is_consuming','true'}}, #state{supervisor=Supervisor}=State) ->
    Self = self(),
    _ = spawn_link(fun() -> notify_of_readiness(Supervisor, Self) end),
    lager:debug("started consuming"),
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
handle_info({'EXIT', _Pid, 'normal'}, State) ->
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{fsm=FsmPid}) ->
    {'reply', [{'fsm', FsmPid}]}.

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
terminate(_Reason, #state{supervisor=Supervisor}) ->
    _ = spawn('ts_offnet_sup', 'stop', [Supervisor]),
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
notify_of_readiness(Supervisor, Listener) ->
    case ts_offnet_sup:fsm(Supervisor) of
        'undefined' ->
            lager:debug("FSM isn't ready yet, wait and try again"),
            timer:sleep(100),
            notify_of_readiness(Supervisor, Listener);
        FSMPid ->
            lager:debug("found fsm ~p", [FSMPid]),
            gen_listener:cast(Listener, {'fsm', FSMPid})
    end.
