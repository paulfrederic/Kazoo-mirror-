%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ks_new_voicemail).

-export([handle_req/2]).

-include("knapsack.hrl").

-record(file, {key
              ,filename
              ,content_type
              ,content
              }).
-type file() :: #file{}.
-type files() :: [files(),...] | [].
-type request_results() :: 'accepted' | 'not_configured' | 'server_error' | 'connection_error'.

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_notifications:voicemail_v(JObj),
    _ = whapps_util:put_callid(JObj),
    lager:debug("new voicemail left, maybe pushing to remote server"),
    case maybe_make_request(JObj) of
        'accepted' -> send_response(JObj);
        _Else -> 'ok'
    end.

-spec send_response(wh_json:object()) -> 'ok'.
send_response(JObj) ->
    Prop = props:filter_undefined(
             [{<<"Status">>, <<"completed">>}
              ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    RespQ = wh_json:get_value(<<"Server-ID">>, JObj),
    lager:debug("sending completed update to ~s", [RespQ]),
    wapi_notifications:publish_notify_update(RespQ, Prop).

-spec maybe_make_request(wh_json:object()) -> request_results().
maybe_make_request(JObj) ->
    case whapps_config:get_non_empty(?CONFIG_CAT, <<"url">>, <<"">>) of
        'undefined' ->
            lager:debug("no remote voicemail push server configured", []),
            'not_configured';
        URL -> make_request(JObj, wh_util:to_list(URL))
    end.

-spec make_request(wh_json:object(), ne_binary()) -> request_results().
make_request(JObj, URL) ->
    Boundary = wh_util:rand_hex_binary(16),
    Body = build_request_body(JObj, <<"--", Boundary/binary>>),
    ContentType = wh_util:to_list(<<"multipart/form-data; boundary=", Boundary/binary>>),
    Headers = [{"Content-Length", integer_to_list(byte_size(Body))}],
    case httpc:request('post', {URL, Headers, ContentType, Body}, [], []) of
        {'ok', {{_, 200, "OK"}, _, _}} ->
            lager:debug("server ~s accepted voicemail", [URL]),
            'accepted';
        {'ok', {{_, _Code, _Reason}, _, _Reply}} ->
            lager:info("server ~s rejected request with ~p ~s: ~p"
                       ,[URL, _Code, _Reason, _Reply]),
            'server_error';
        {'error', _Reason} ->
            lager:info("unable to reach server ~s: ~p"
                       ,[URL, _Reason]),
            'connection_error'
    end.

-spec build_request_body(wh_json:object(), ne_binary()) -> ne_binary().
build_request_body(JObj, Boundary) ->
    Props = create_metadata(JObj),
    File = get_voicemail_content(JObj, Props),
    format_multipart(Props, File, Boundary).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec format_multipart(wh_proplist(), files(), ne_binary()) -> ne_binary().
format_multipart(Props, File, Boundary) ->
    Payload = format_multipart_file(File, Boundary),
    format_multipart_props(Props, Boundary, Payload).

-spec format_multipart_props(wh_proplist(), ne_binary(), ne_binary()) -> ne_binary().
format_multipart_props([], _, Payload) -> Payload;
format_multipart_props([{Key, Value}|Props], Boundary, Payload) ->
    P = <<(format_multipart_formdata(Key, Value, Boundary))/binary, Payload/binary>>,
    format_multipart_props(Props, Boundary, P).

-spec format_multipart_formdata(ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
format_multipart_formdata(Key, Value, Boundary) ->
    <<"Content-Disposition: form-data; name=\"", Key/binary, "\"\r\n\r\n"
      ,(wh_util:to_binary(Value))/binary, "\r\n"
      ,Boundary/binary, "\r\n">>.

-spec format_multipart_file(file(), ne_binary()) -> ne_binary().
format_multipart_file(#file{key=Key
                           ,filename=Filename
                           ,content_type=ContentType
                           ,content=Content
                           }, Boundary) ->
    <<"Content-Disposition: form-data; name=\"", Key/binary, "\"; filename=\"", Filename/binary, "\"\r\n"
      ,"Content-Type: ", ContentType/binary, "\r\n\""
      ,"Content-Transfer-Encoding: binary\r\n\r\n"
      ,Content/binary, "\r\n"
      ,Boundary/binary, "--\r\n\r\n">>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_voicemail_content(wh_json:object(), wh_proplist()) -> file().
get_voicemail_content(JObj, Props) ->
    AccountDb = wh_json:get_value(<<"Account-DB">>, JObj),
    VoicemailId = wh_json:get_value(<<"Voicemail-Name">>, JObj),
    {'ok', VoicemailJObj} = couch_mgr:open_cache_doc(AccountDb, VoicemailId),
    [AttachmentId|_] = wh_json:get_keys(<<"_attachments">>, VoicemailJObj),
    ContentType = wh_json:get_value([<<"_attachments">>
                                     ,AttachmentId
                                     ,<<"content_type">>
                                    ], VoicemailJObj, <<"application/octet-stream">>),
    lager:debug("attempting to fetch ~s/~s attachment id ~s"
               ,[AccountDb, VoicemailId, AttachmentId]),
    {'ok', Content} = couch_mgr:fetch_attachment(AccountDb, VoicemailId, AttachmentId),
    #file{key = wh_json:get_value(<<"pvt_type">>, VoicemailJObj)
         ,filename = create_file_name(JObj, Props)
         ,content_type = ContentType
         ,content = Content
         }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec create_metadata(wh_json:object()) -> wh_proplist().
create_metadata(JObj) ->
    AccountDb = wh_json:get_value(<<"Account-DB">>, JObj),
    MailboxId = wh_json:get_value(<<"Voicemail-Box">>, JObj),
    {'ok', MailboxJObj} = couch_mgr:open_cache_doc(AccountDb, MailboxId),
    UserJObj = maybe_get_user(AccountDb
                             ,wh_json:get_value(<<"owner_id">>, MailboxJObj)),
    props:filter_undefined(
      [{<<"name">>, 'undefined'}
      ,{<<"description">>, <<"voicemail message media">>}
      ,{<<"source_type">>, <<"voicemai">>}
      ,{<<"media_source">>, <<"call">>}
      ,{<<"media_filename">>, 'undefined'}
      ,{<<"streamable">>, <<"true">>}
      ,{<<"from_user">>, wh_json:get_value(<<"From-User">>, JObj)}
      ,{<<"from_realm">>, wh_json:get_value(<<"From-Realm">>, JObj)}
      ,{<<"to_user">>, wh_json:get_value(<<"To-User">>, JObj)}
      ,{<<"to_realm">>, wh_json:get_value(<<"To-Realm">>, JObj)}
      ,{<<"caller_id_number">>, wh_json:get_value(<<"Caller-ID-Number">>, JObj)}
      ,{<<"caller_id_name">>, wh_json:get_value(<<"Caller-ID-Name">>, JObj)}
      ,{<<"call_id">>, wh_json:get_value(<<"Call-ID">>, JObj)}
      ,{<<"voicemail_box_id">>, wh_json:get_value(<<"Voicemail-Box">>, JObj)}
      ,{<<"voicemail_length">>, wh_json:get_value(<<"Voicemail-Length">>, JObj)}
      ,{<<"voicemail_timestamp">>, wh_json:get_value(<<"Voicemail-Timestamp">>, JObj)}
      ,{<<"owner_id">>, wh_json:get_value(<<"_id">>, UserJObj)}
      ,{<<"timezone">>, wh_json:find(<<"timezone">>, [UserJObj, MailboxJObj])}
      ]).

-spec maybe_get_user(ne_binary(), api_binary()) -> wh_json:object().
maybe_get_user(_, 'undefined') -> wh_json:new();
maybe_get_user(AccountDb, OwnerId) ->
    case couch_mgr:open_cache_doc(AccountDb, OwnerId) of
        {'ok',  JObj} -> JObj;
        {'error', _R} ->
            lager:debug("unable to open owner doc ~s/~s"
                       ,[AccountDb, OwnerId]),
            wh_json:new()
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec create_file_name(wh_json:object(), wh_proplist()) -> ne_binary().
create_file_name(JObj, Props) ->
    %% CallerID_Date_Time.mp3
    CallerID = get_caller_id(JObj),
    LocalDateTime = wh_util:pretty_print_datetime(get_local_date_time(JObj, Props)),
    Filename = list_to_binary([CallerID, "_", LocalDateTime, ".mp3"]),
    binary:replace(wh_util:to_lower_binary(Filename), <<" ">>, <<"_">>).

-spec get_caller_id(wh_json:object()) -> ne_binary().
get_caller_id(JObj) ->
    case {wh_json:get_ne_value(<<"Caller-ID-Name">>, JObj)
         ,wh_json:get_ne_value(<<"Caller-ID-Number">>, JObj)}
    of
        {'undefined', 'undefined'} -> <<"Unknown">>;
        {'undefined', Num} -> wnm_util:pretty_print(wh_util:to_binary(Num));
        {Name, _} -> wnm_util:pretty_print(wh_util:to_binary(Name))
    end.

-spec get_local_date_time(wh_json:object(), wh_proplist()) -> any().
get_local_date_time(JObj, Props) ->
    Timestamp = wh_json:get_integer_value(<<"Voicemail-Timestamp">>, JObj),
    DateTime = calendar:gregorian_seconds_to_datetime(Timestamp),
    Timezone = wh_util:to_list(props:get_value(<<"timezone">>, Props, <<"UTC">>)),
    ClockTimezone = whapps_config:get_string(<<"servers">>, <<"clock_timezone">>, <<"UTC">>),
    localtime:local_to_local(DateTime, ClockTimezone, Timezone).
