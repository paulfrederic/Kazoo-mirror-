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

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_notifications:voicemail_v(JObj),
    _ = whapps_util:put_callid(JObj),
    lager:debug("new voicemail left"),
    maybe_make_request(JObj).

-spec maybe_make_request(wh_json:object()) -> 'ok'.
maybe_make_request(JObj) ->
    URL = whapps_config:get_value(?CONFIG_CAT, <<"">>),
    case wh_util:is_empty(URL) of
        'true' -> 'ok';
        'false' -> make_request(JObj, URL)
    end.

-spec make_request(wh_json:object(), ne_binary()) -> ne_binary().
make_request(JObj, URL) ->
    Payload = build_request_payload(JObj).

-spec build_request_payload(wh_json:object()) -> ne_binary().
build_request_payload(JObj) ->
    Props = create_metadata(JObj),
    File = get_voicemail_content(JObj, Props),
    format_multipart(Props, [File]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec format_multipart(wh_proplist(), files()) -> ne_binary().
format_multipart(Props, Files) ->
    Boundary = wh_util:rand_hex_binary(16),
    Routines = [fun(Payload) -> format_multipart_props(Props, Boundary, Payload) end
                ,fun(Payload) -> format_multipart_files(Files, Boundary, Payload) end
               ],
    lists:foldr(fun(F, P) -> F(P) end, <<>>, Routines).

-spec format_multipart_props(wh_proplist(), ne_binary(), ne_binary()) -> ne_binary().
format_multipart_props([], _, Payload) -> Payload;
format_multipart_props([{Key, Value}|Props], Boundary, Payload) ->
    C = <<(format_multipart_formdata(Key, Value, Boundary))/binary, Payload>>,
    format_multipart_props(Props, Boundary, C).

-spec format_multipart_formdata(ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
format_multipart_formdata(Key, Value, Boundary) ->
    <<"Content-Disposition: form-data; name=\"", Key/binary, "\"\r\n"
      ,Value/binary, "\r\n"
      ,Boundary/binary, "\r\n">>.

-spec format_multipart_files(files(), ne_binary(), ne_binary()) -> ne_binary().
format_multipart_files([], Boundary, Payload) -> Payload;
format_multipart_files([File|Files], Boundary, Payload) ->
    C = <<(format_multipart_file(File, Boundary))/binary
          ,Payload>>,
    format_multipart_files(Files, Boundary, Payload).

-spec format_multipart_file(file(), ne_binary()) -> ne_binary().
format_multipart_file(#file{key=Key
                           ,filename=Filename
                           ,content_type=ContentType
                           ,content=Content
                           }, Boundary) ->
    <<"Content-Disposition: form-data; name=\"", Key/binary, "\"; filename=\"", Filename/binary, "\"\r\n"
      ,"Content-Type: ", ContentType/binary, "\r\n"
      ,"Content-Transfer-Encoding: binary\r\n"
      ,Content/binary, "\r\n"
      ,Boundary/binary, "\r\n">>.

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
    {'ok', JObj} = couch_mgr:open_cache_doc(AccountDb, VoicemailId),
    [AttachmentId|_] = wh_json:get_keys(<<"_attachments">>, JObj),
    ContentType = wh_json:get_value([<<"_attachments">>
                                    ,AttachmentId
                                    ,<<"content_type">>
                                    ], JObj, <<"application/octet-stream">>),
    lager:debug("attempting to fetch ~s/~s attachment id ~s"
               ,[AccountDb, VoicemailId, AttachmentId]),
    {'ok', Content} = couch_mgr:fetch_attachment(AccountDb, VoicemailId, AttachmentId),
    #file{key = wh_json:get_value(<<"pvt_type">>)
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
      [{<<"name">>, <<>>}
      ,{<<"description">>, <<"voicemail message media">>}
      ,{<<"source_type">>, <<"voicemai">>}
      ,{<<"media_source">>, <<"call">>}
      ,{<<"media_filename">>, <<>>}
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
    case {props:get_value(<<"Caller-ID-Name">>, JObj)
         ,props:get_value(<<"Caller-ID-Number">>, JObj)}
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

-spec preaty_print_length('undefined' | integer() | wh_json:object()) -> ne_binary().
preaty_print_length('undefined') -> <<"00:00">>;
preaty_print_length(Milliseconds) when is_integer(Milliseconds) ->
    Seconds = round(Milliseconds / 1000) rem 60,
    Minutes = trunc(Milliseconds / (1000*60)) rem 60,
    wh_util:to_binary(io_lib:format("~2..0w:~2..0w", [Minutes, Seconds]));
preaty_print_length(Event) ->
    preaty_print_length(wh_json:get_integer_value(<<"Voicemail-Length">>, Event)).
