%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%% Handle route requests off AMQP
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ts_route_req).

-export([init/0
         ,handle_req/2
        ]).

-include("ts.hrl").

init() -> 'ok'.

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    'true' = wapi_route:req_v(JObj),

    Call = whapps_call:set_controller_queue(
             props:get_value('queue', Props)
             ,whapps_call:from_route_req(JObj)
            ),
    whapps_call:put_callid(Call),

    case is_binary(whapps_call:account_id(Call))
        andalso trunkstore_should_respond(Call)
    of
        %% ignore, stepswitch will add information for us
        'false' -> 'ok';
        'true' -> send_response(JObj, Call)
    end.

-spec trunkstore_should_respond(whapps_call:call()) -> boolean().
trunkstore_should_respond(Call) ->
    Fs = [fun is_trunkstore_acct/1
          ,fun(C) -> is_trunkstore_did(C)
                         orelse is_trunkstore_user(C)
           end
         ],
    lists:all(fun(F) -> F(Call) end, Fs).

-spec is_trunkstore_acct(whapps_call:call()) -> boolean().
is_trunkstore_acct(Call) ->
    case whapps_call:authorizing_type(Call) of
        'undefined' -> 'true';
        <<"sys_info">> -> 'true';
        _ -> 'false'
    end.

-spec is_trunkstore_did(whapps_call:call()) -> boolean().
is_trunkstore_did(Call) ->
    ToDID = wnm_util:to_e164(whapps_call:to_user(Call)),
    case ts_util:lookup_did(ToDID, whapps_call:account_id(Call)) of
        {'ok', _} -> 'true';
        {'error', _E} ->
            lager:debug("failed to lookup DID ~s: ~p", [ToDID, _E]),
            'false'
    end.

is_trunkstore_user(Call) ->
    case couch_mgr:open_cache_doc(whapps_call:account_db(Call)
                                  ,whapps_call:authorizing_id(Call)
                                 )
    of
        {'ok', D} ->
            wh_json:get_value(<<"pvt_type">>, D) =:= <<"sys_info">>;
        {'error', _E} ->
            lager:debug("failed to open authorizing id doc ~s(~s): ~p"
                        ,[whapps_call:authorizing_id(Call)
                          ,whapps_call:account_id(Call)
                          ,_E
                         ]),
            'false'
    end.

-spec send_response(wh_json:object(), whapps_call:call()) -> 'ok'.
send_response(JObj, Call) ->
    Resp = props:filter_undefined(
             [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
              ,{<<"Routes">>, []}
              ,{<<"Method">>, <<"park">>}
              ,{<<"From-Realm">>, wh_util:get_account_realm(whapps_call:account_id(Call))}
              | wh_api:default_headers(whapps_call:controller_queue(Call), ?APP_NAME, ?APP_VERSION)
             ]),
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    Publisher = fun(P) -> wapi_route:publish_resp(ServerId, P) end,
    whapps_util:amqp_pool_send(Resp, Publisher),
    lager:info("trunkstore knows how to route the call! sent park response").
