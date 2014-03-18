%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(ks_new_voicemail).

-export([handle_req/2]).

-include("knapsack.hrl").

-spec handle_req(wh_json:object(), wh_proplist()) -> any().
handle_req(JObj, Props) ->
    'true' = wapi_notifications:voicemail_v(JObj),
    _ = whapps_util:put_callid(JObj),
    Account = wh_json:get_first_defined([<<"Account-ID">>
                                         ,<<"Account-DB">>
                                        ], JObj),
    VoicemailBox = wh_json:get_value(<<"Voicemail-Box">>, JObj),
    OwnerId = wh_json:get_value(<<"Owner-ID">>, JObj),
    Configuration = kz_utils:get_configuration(Account, [OwnerId, VoicemailBox]),
    {'ok', Engine} = kz_utils:get_engine(Configuration, <<"voicemail">>),
    Metadata = new_voicemail_metadata(),
    Content = get_voicemail_message(),
    ContentType = get_content_type(),
    case Engine:store(Content, ContentType, Metadata, Options) of
        {'ok', Identifier} -> send_success(Identifier, JObj);
        {'error', _} -> 'ok'
    end.

new_voicemail_metadata() ->
    [].

get_voicemail_message() ->
    <<>>.

get_content_type() ->
    <<"audio/x-wav">>.
