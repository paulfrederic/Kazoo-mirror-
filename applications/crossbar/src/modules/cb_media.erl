%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz
%%% @doc
%%% Account module
%%%
%%% Store/retrieve media files
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_media).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,validate/1, validate/2, validate/3
         ,content_types_provided/3
         ,content_types_accepted/3
         ,put/1
         ,post/2, post/3
         ,delete/2, delete/3
        ]).

-include("../crossbar.hrl").

-define(SERVER, ?MODULE).
-define(BIN_DATA, <<"raw">>).

-define(MEDIA_MIME_TYPES, [{<<"audio">>, <<"x-wav">>}
                           ,{<<"audio">>, <<"wav">>}
                           ,{<<"audio">>, <<"mpeg">>}
                           ,{<<"audio">>, <<"mp3">>}
                           ,{<<"audio">>, <<"ogg">>}
                           ,{<<"application">>, <<"base64">>}
                           ,{<<"application">>, <<"x-base64">>}
                          ]).

-define(CB_LIST, <<"media/crossbar_listing">>).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".media">>).
-define(DEFAULT_VOICE, whapps_config:get(<<"speech">>, <<"tts_default_voice">>, <<"female/en-US">>)).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.content_types_provided.media">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.media">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.media">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.media">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.media">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.media">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.media">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.media">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_MediaID) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].
allowed_methods(_MediaID, ?BIN_DATA) ->
    [?HTTP_GET, ?HTTP_POST].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_, ?BIN_DATA) -> 'true'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
-spec content_types_provided(cb_context:context(), path_token(), path_token(), http_method()) ->
                                    cb_context:context().
content_types_provided(Context, MediaId, ?BIN_DATA) ->
    content_types_provided(Context, MediaId, ?BIN_DATA, cb_context:req_verb(Context)).

content_types_provided(Context, MediaId, ?BIN_DATA, ?HTTP_GET) ->
    AccountDb = cb_context:account_db(Context),
    case wh_media:fetch(AccountDb, MediaId) of
        {'error', _} -> Context;
        Media ->
            ContentType = wh_media:content_type(Media),
            cb_context:set_content_types_provided(Context, [{'to_binary', [ContentType]}])
    end;
content_types_provided(Context, _MediaId, ?BIN_DATA, _Verb) ->
    Context.

-spec content_types_accepted(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
-spec content_types_accepted_for_upload(cb_context:context(), http_method()) ->
                                               cb_context:context().
content_types_accepted(Context, _MediaId, ?BIN_DATA) ->
    content_types_accepted_for_upload(Context, cb_context:req_verb(Context)).

content_types_accepted_for_upload(Context, ?HTTP_POST) ->
    CTA = [{'from_binary', ?MEDIA_MIME_TYPES}],
    cb_context:set_content_types_accepted(Context, CTA);
content_types_accepted_for_upload(Context, _Verb) ->
    Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_media_docs(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, MediaId) ->
    validate_media_doc(Context, MediaId, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, MediaId, ?BIN_DATA) ->
    validate_media_binary(Context, MediaId, cb_context:req_verb(Context)).

validate_media_docs(Context, ?HTTP_GET) ->
    load_media_summary(Context);
validate_media_docs(Context, ?HTTP_PUT) ->
    validate_request('undefined', Context).

validate_media_doc(Context, MediaId, ?HTTP_GET) ->
    load_media_meta(MediaId, Context);
validate_media_doc(Context, MediaId, ?HTTP_POST) ->
    validate_request(MediaId, Context);
validate_media_doc(Context, MediaId, ?HTTP_DELETE) ->
    load_media_meta(MediaId, Context).

validate_media_binary(Context, MediaId, ?HTTP_GET) ->
    load_media_binary(MediaId, Context);
validate_media_binary(Context, MediaId, ?HTTP_POST) ->
    case cb_context:req_files(Context) of
        [{_, _}] -> load_media_meta(MediaId, Context);
        _Else ->
            Message = <<"please provide a single media file">>,
            cb_context:add_validation_error(<<"file">>, <<"required">>, Message, Context)
    end;
validate_media_binary(Context, MediaId, ?HTTP_DELETE) ->
    load_media_meta(MediaId, Context).


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_request(api_binary(), cb_context:context()) -> cb_context:context().
validate_request(MediaId, Context) ->
    check_media_schema(MediaId, Context).

check_media_schema(MediaId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(MediaId, C) end,
    cb_context:validate_request_data(<<"media">>, Context, OnSuccess).

on_successful_validation('undefined', Context) ->
    AccountDb = cb_context:account_db(Context),    
    JObj = wh_json:set_value(<<"media_source">>
                             ,<<"crossbar">>
                             ,cb_context:doc(Context)),
    cb_context:set_doc(Context, wh_media:new_public(AccountDb, JObj));
on_successful_validation(MediaId, Context) ->
    AccountDb = cb_context:account_db(Context),
    case wh_media:fetch(AccountDb, MediaId) of
        {'error', _}=Error -> 
            %% TODO: convert to cb_context error
            Error;
        Media ->
            JObj = cb_context:doc(Context),
            cb_context:set_doc(Context, wh_media:merge_metadata(JObj, Media))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    case wh_media:update(cb_context:doc(Context)) of
        {'error', _}=Error -> 
            %% TODO: convert to cb_context error
            Error;
        Media ->
            JObj = wh_media:metadata(Media),
            cb_context:success_updaters(JObj, Context)
    end.        

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _MediaId) ->
    case wh_media:update(cb_context:doc(Context)) of
        {'error', _}=Error -> 
            %% TODO: convert to cb_context error
            Error;
        Media ->
            JObj = wh_media:metadata(Media),
            cb_context:success_updaters(JObj, Context)
    end.

-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, _MediaId, ?BIN_DATA) ->
    [{Filename, File}] = cb_context:req_files(Context),
    ContentType = wh_json:get_value([<<"headers">>, <<"content_type">>], File),
    ContentLength = wh_json:get_integer_value([<<"headers">>, <<"content_length">>], File),
    MediaName = cb_modules_util:attachment_name(Filename, ContentType),
    Content = wh_json:get_value(<<"contents">>, File),
    Routines = [fun(M) -> wh_media:set_content_type(ContentType, M) end
                ,fun(M) -> wh_media:set_content_length(ContentLength, M) end
                ,fun(M) -> wh_media:set_media_name(MediaName, M) end
                ,fun(M) -> wh_media:set_content(Content, M) end
               ],
    case wh_media:store(lists:foldl(fun(F, M) -> F(M) end, cb_context:doc(Context), Routines)) of
        {'error', _}=Error -> 
            %% TODO: convert to cb_context error
            Error;
        Media ->
            JObj = wh_media:metadata(Media),
            cb_context:success_updaters(JObj, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _MediaID) ->
    Media = cb_context:doc(Context),
    case wh_media:delete(Media) of
        {'error', _}=Error ->
            %% TODO: convert to cb_context error
            Error;
        'ok' ->
            JObj = wh_media:metadata(Media),
            cb_context:success_updaters(JObj, Context)
    end.

-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, _MediaID, ?BIN_DATA) ->
    case wh_media:delete_content(cb_context:doc(Context)) of
        {'error', _}=Error ->
            %% TODO: convert to cb_context error
            Error;
        Media ->
            JObj = wh_media:metadata(Media),
            cb_context:success_updaters(JObj, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec load_media_meta(ne_binary(), cb_context:context()) -> cb_context:context().
load_media_meta(MediaId, Context) ->
    AccountDb = cb_context:account_db(Context),
    case wh_media:fetch(AccountDb, MediaId) of
        {'error', _}=Error -> 
            %% TODO: convert to cb_context error
            Error;
        Media ->
            JObj = wh_json:public_fields(wh_media:metadata(Media)),
            Routines = [fun(C) -> cb_context:set_resp_data(C, JObj) end
                        ,fun(C) -> cb_context:set_resp_status(C, 'success') end
                        ,fun(C) -> cb_context:set_doc(C, Media) end
                       ],
            lists:foldl(fun(F, C) -> F(C) end, Context, Routines)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load the binary attachment of a media doc
%% @end
%%--------------------------------------------------------------------
-spec load_media_binary(path_token(), cb_context:context()) -> cb_context:context().
load_media_binary(MediaId, Context) ->
    AccountDb = cb_context:account_db(Context),
    case wh_media:fetch(AccountDb, MediaId) of
        {'error', _}=Error ->
            %% TODO: convert to cb_context error
            Error;
        Media ->
            Routines = [fun(C) -> cb_context:set_resp_data(C, wh_media:content(Media)) end
                        ,fun(C) -> cb_context:set_resp_status(C, 'success') end
                        ,fun(C) -> cb_context:set_doc(C, Media) end
                        ,fun(C) ->
                                 Filename = wh_media:filename(Media),
                                 Props = [{<<"Content-Disposition">>, <<"attachment; filename=", Filename/binary>>}
                                          ,{<<"Content-Type">>, wh_media:content_type_binary(Media)}
                                          ,{<<"Content-Length">>, wh_media:content_length(Media)}
                                         ],
                                 cb_context:add_resp_headers(C, Props)
                         end
                       ],
            lists:foldl(fun(F, C) -> F(C) end, Context, Routines)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized list of media
%% @end
%%--------------------------------------------------------------------
-spec load_media_summary(cb_context:context()) -> cb_context:context().
load_media_summary(Context) ->
    %% TODO: move this to wh_media...
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

-spec normalize_view_results(wh_json:object(), wh_json:objects()) ->
                                    wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].
