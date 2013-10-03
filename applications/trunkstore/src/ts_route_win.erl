%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%% Handle route requests off AMQP
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ts_route_win).

-export([init/0
         ,handle_req/2
        ]).

-include("ts.hrl").

init() -> 'ok'.

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    put('callid', CallId),
    lager:info("callflow has received a route win, taking control of the call"),
    case whapps_call:retrieve(CallId) of
        {'ok', Call} ->
            process_call(whapps_call:from_route_win(JObj, Call));
        {'error', _R} ->
            lager:info("unable to find call after recv route_win (HUH?): ~p", [_R])
    end.

process_call(Call) ->
    case whapps_call:inception(Call) of
        <<"on-net">> -> ts_onnet_sup:new(Call);
        <<"off-net">> -> ts_offnet_sup:new(Call)
    end.
