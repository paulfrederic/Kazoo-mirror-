%%%-------------------------------------------------------------------
%%% @copyright (C) 2014 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(wh_media_bigcouch).

-export([fetch/1]).
-export([fetch_url/1]).
-export([content/1]).
-export([store/1]).
-export([store_url/1]).
-export([delete_content/1]).
-export([delete/1]).

-include("../whistle_media.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch(wh_media:media()) -> wh_media:media().
fetch(Media) ->
    case wh_media:media_name(Media) of
        'undefined' -> maybe_set_media_name(Media);
        _Else -> maybe_set_content_info(Media)
    end.

-spec maybe_set_media_name(wh_media:media()) -> wh_media:media().
maybe_set_media_name(Media) ->
    Metadata = wh_media:metadata(Media),
    case wh_json:get_keys(<<"_attachments">>, Metadata) of
        [] -> Media;
        [MediaName|_] ->
            set_content_info(MediaName, wh_media:set_media_name(MediaName, Media))
    end.

-spec maybe_set_content_info(wh_media:media()) -> wh_media:media().
maybe_set_content_info(Media) ->
   case wh_media:media_name(Media) of
       'undefined' -> Media;
       MediaName -> set_content_info(MediaName, Media)
   end.

-spec set_content_info(ne_binary(), wh_media:media()) -> wh_media:media().
set_content_info(MediaName, Media) ->
    Metadata = wh_media:metadata(Media),
    ContentType = wh_json:get_value([<<"_attachments">>
                                     ,MediaName
                                     ,<<"content_type">>
                                    ], Metadata),
    ContentLength = wh_json:get_integer_value([<<"_attachments">>
                                               ,MediaName
                                               ,<<"length">>
                                              ], Metadata),
    Routines = [fun(M) -> wh_media:set_content_type(ContentType, M) end
                ,fun(M) -> wh_media:set_content_length(ContentLength, M) end
               ],
    lists:foldl(fun(F, M) -> F(M) end, Media, Routines).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec fetch_url(wh_media:media()) -> wh_media:media().
fetch_url(Media) ->
    %% TODO: fix hardcoded value
    <<"http://127.0.0.1:15984"
      ,$/, (wh_media:account_db(Media))/binary
      ,$/, (wh_media:metadata_id(Media))/binary
      ,$/, (wh_media:media_name(Media))/binary
    >>.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec content(wh_media:media()) -> wh_media:media().
content(Media) ->
    AccountDb = wh_media:account_db(Media),
    MetadataId = wh_media:metadata_id(Media),
    MediaName = wh_media:media_name(Media),
    case couch_mgr:fetch_attachment(AccountDb, MetadataId, MediaName) of
        {'error', _}=Error ->  Error;
        {'ok', Content} -> Content
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec store(wh_media:media()) -> wh_media:media().
store(Media) ->
    AccountDb = wh_media:account_db(Media),
    MetadataId = wh_media:metadata_id(Media),
    MediaName = wh_media:media_name(Media),
    Content = wh_media:content(Media),
    ContentType = wh_media:content_type_binary(Media),
    Props = [{'headers', [{'content_type', wh_util:to_list(ContentType)}]}],
    case couch_mgr:put_attachment(AccountDb, MetadataId, MediaName, Content, Props) of
        {'error', _}=Error -> Error;
        {'ok', _} -> cleanup(Media)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec store_url(wh_media:media()) -> ne_binary().
store_url(Media) -> 
    <<>>.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_content(wh_media:media()) -> wh_media:media().
delete_content(Media) ->
    AccountDb = wh_media:account_db(Media),
    MetadataId = wh_media:metadata_id(Media),
    case couch_mgr:open_doc(AccountDb, MetadataId) of
        {'error', _R}=Error -> Error;
        {'ok', JObj} ->
            J = wh_json:delete_key(<<"_attachments">>, JObj),
            case couch_mgr:save_doc(AccountDb, J) of
                {'error', _}=Error -> Error;
                {'ok', Metadata} -> wh_media:set_metadata(Metadata, Media)
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete(wh_media:media()) -> 'ok'.
delete(_Media) -> 'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cleanup(wh_media:media()) -> wh_media:media().
cleanup(Media) ->
    AccountDb = wh_media:account_db(Media),
    MetadataId = wh_media:metadata_id(Media),
    case couch_mgr:open_doc(AccountDb, MetadataId) of
        {'error', _R} ->
            lager:info("unable to clean up metadata after upload: ~p", [_R]),
            Media;
        {'ok', JObj} ->
            MediaName = wh_media:media_name(Media),
            Attachment = wh_json:get_value([<<"_attachments">>, MediaName], JObj),
            J = wh_json:set_value(<<"_attachments">>, wh_json:from_list([{MediaName, Attachment}]), JObj),
            case couch_mgr:save_doc(AccountDb, J) of
                {'error', _R} ->
                    lager:info("unable to remove other attachments: ~p", [_R]),
                    Media;
                {'ok', Metadata} -> wh_media:set_metadata(Metadata, Media)
            end
    end.
