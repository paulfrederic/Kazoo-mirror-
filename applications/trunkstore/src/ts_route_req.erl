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

{ok,{[{<<"_id">>,<<"1443ee2970322db9bdd51a8cf93042e9">>},
      {<<"_rev">>,<<"36-bd6af7cdc3ac1c20b4bdabd6a601a178">>},
      {<<"account">>,
       {[{<<"credits">>,{[{<<"prepay">>,<<"0.00">>}]}},
         {<<"trunks">>,<<"0">>},
         {<<"inbound_trunks">>,<<"0">>},
         {<<"auth_realm">>,<<"17efa7.sip.2600hz.com">>}]}},
      {<<"billing_account_id">>,
       <<"b10d34188f6ed382c18c1102bf1eebc1">>},
      {<<"DIDs_Unassigned">>,{[]}},
      {<<"servers">>,
       [{[{<<"DIDs">>,
           {[{<<"+13177089001">>,{[]}},
             {<<"+17653034364">>,{[]}},
             {<<"+13177084949">>,{[]}},
             {<<"+17653487316">>,{[]}},
             {<<"+17653487322">>,{[]}},
             {<<"+13179269000">>,{[]}},
             {<<"+13179265980">>,{[]}},
             {<<"+14142399229">>,{[]}},
             {<<"+1317279"...>>,{[]}},
             {<<"+185"...>>,{...}},
             {<<...>>,...},
             {...}|...]}},
          {<<"options">>,
           {[{<<"enabled">>,true},
             {<<"inbound_format">>,<<"e.164">>},
             {<<"international">>,false},
             {<<"caller_id">>,{[]}},
             {<<"e911_info">>,{[]}},
             {<<"failover">>,{[]}},
             {<<"media_handli"...>>,<<"process">>}]}},
          {<<"permissions">>,{[{<<"users">>,[]}]}},
          {<<"monitor">>,{[{<<"monitor_enabled">>,false}]}},
          {<<"auth">>,
           {[{<<"auth_user">>,<<"voicelogic">>},
             {<<"auth_password">>,<<"powerthoughts201"...>>},
             {<<"auth_method">>,<<"Password">>}]}},
          {<<"server_name">>,<<"Cluster1">>},
          {<<"server_type">>,<<"FreeSWITCH">>}]},
        {[{<<"DIDs">>,{[]}},
          {<<"options">>,
           {[{<<"enabled">>,true},
             {<<"inbound_format">>,<<"e.164">>},
             {<<"international">>,false},
             {<<"caller_id">>,{[]}},
             {<<"e911_info">>,{[]}},
             {<<"failover">>,{[]}},
             {<<"media_ha"...>>,<<"bypa"...>>}]}},
          {<<"permissions">>,{[{<<"users">>,[]}]}},
          {<<"monitor">>,{[{<<"monitor_enabled">>,false}]}},
          {<<"auth">>,
           {[{<<"auth_user">>,<<"faxage">>},
             {<<"auth_password">>,<<"v0xLogic!">>},
             {<<"auth_method">>,<<"Password">>}]}},
          {<<"server_name">>,<<"Fax Server">>},
          {<<"server_type">>,<<"FreeSWITCH">>}]}]},
      {<<"id">>,<<"1443ee2970322db9bdd51a8cf93042e9">>},
      {<<"pvt_type">>,<<"sys_info">>},
      {<<"pvt_modified">>,63538972677},
      {<<"pvt_created">>,63511791356},
      {<<"pvt_account_db">>,
       <<"account%2Fb1%2F0d%2F34188f6ed382c18c1102bf1eebc1">>},
      {<<"pvt_account_id">>,
       <<"b10d34188f6ed382c18c1102bf1eebc1">>},
      {<<"pvt_vsn">>,<<"1">>}]}}
