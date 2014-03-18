%%%-------------------------------------------------------------------
%%% @copyright (C) 2014 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(wh_media).

-export([new_public/1
         ,new_public/2
         ,new_private/1
         ,new_private/2
        ]).
-export([fetch/2
         ,fetch/3
        ]).
-export([fetch_url/1]).
-export([account_db/1]).
-export([account_id/1]).
-export([metadata_id/1]).
-export([content_type/1
         ,content_type_binary/1
         ,set_content_type/2
         ,set_content_type/3
        ]).
-export([content_length/1
         ,set_content_length/2
        ]).
-export([media_name/1
         ,set_media_name/2
        ]).
-export([filename/1
         ,set_filename/2
        ]).
-export([metadata/1
         ,set_metadata/2
         ,merge_metadata/2
        ]).
-export([content/1
         ,set_content/2
        ]).
-export([update/1]).
-export([store/1]).
-export([store_url/1]).
-export([delete_content/1
         ,delete/1
        ]).

-record(wh_media, {account_id
                   ,account_db
                   ,metadata_id
                   ,media_name
                   ,metadata
                   ,engine

                   ,content
                   ,content_type
                   ,content_subtype
                   ,content_length
                   ,filename}).

-type wh_media() :: #wh_media{}.

-include("whistle_media.hrl").

new_public(Account) ->
    new_public(Account, wh_json:new()).

new_public(Account, JObj) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    AccountId = wh_util:format_account_id(Account, 'raw'),
    Timestamp = wh_util:current_tstamp(),
    Props = [{<<"pvt_type">>, <<"media">>}
             ,{<<"pvt_account_db">>, AccountDb}
             ,{<<"pvt_account_id">>, AccountId}
             ,{<<"pvt_created">>, Timestamp}
            ],
    #wh_media{account_db=AccountDb
              ,account_id=AccountId
              ,metadata=wh_json:set_values(Props, wh_json:public_fields(JObj))
              ,engine=get_engine('undefined')
             }.

new_private(Account) ->
    new_private(Account, wh_json:new()).

new_private(Account, JObj) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    AccountId = wh_util:format_account_id(Account, 'raw'),
    Timestamp = wh_util:current_tstamp(),
    Props = [{<<"pvt_type">>, <<"private_media">>}
             ,{<<"pvt_account_db">>, AccountDb}
             ,{<<"pvt_account_id">>, AccountId}
             ,{<<"pvt_created">>, Timestamp}
            ],
    #wh_media{account_db=AccountDb
              ,account_id=AccountId
              ,metadata=wh_json:set_values(Props, wh_json:public_fields(JObj))
              ,engine=get_engine('undefined')
             }.

-spec fetch(ne_binary(), ne_binary()) -> wh_media() | {'error', _}.
fetch(Account, Id) ->
    fetch(Account, Id, 'undefined').

-spec fetch(ne_binary(), ne_binary(), api_binary()) -> wh_media() | {'error', _}.    
fetch(Account, Id, MediaName) ->
    Media = 
        #wh_media{account_id=wh_util:format_account_id(Account, 'raw')
                  ,account_db=wh_util:format_account_id(Account, 'encoded')
                  ,metadata_id=Id
                  ,media_name=MediaName
                 },
    case load_metadata(Media) of
        {'error', _}=Error -> Error;
        {'ok', JObj} ->
            Engine = get_engine(JObj),
            Engine:fetch(Media#wh_media{metadata=JObj
                                        ,engine=Engine})
    end.

-spec fetch_url(wh_media()) -> ne_binary().
fetch_url(#wh_media{engine=Engine}=Media) ->
    Engine:fetch_url(Media).

reload(Media) -> Media.
     
-spec account_db(wh_media()) -> api_binary().
account_db(#wh_media{account_db=AccountDb}) -> AccountDb.

-spec account_id(wh_media()) -> api_binary().
account_id(#wh_media{account_id=AccountId}) -> AccountId.

-spec metadata_id(wh_media()) -> api_binary().
metadata_id(#wh_media{metadata_id=MetadataId}) -> MetadataId.

-spec content_type(wh_media()) -> 'undefined' | {ne_binary(), ne_binary()}.
content_type(#wh_media{content_type='undefined'}) -> 'undefined';
content_type(#wh_media{content_subtype='undefined'}) -> 'undefined';
content_type(#wh_media{content_type=Type
                       ,content_subtype=Subtype}) ->
    {Type, Subtype}.

-spec content_type_binary(wh_media()) -> api_binary().
content_type_binary(Media) ->
    case content_type(Media) of
        'undefined' -> 'undefined';
        {Type, Subtype} ->
            <<Type/binary, "/", Subtype/binary>>
    end.

set_content_type('undefined', Media) ->
    Media#wh_media{content_type='undefined'
                   ,content_subtype='undefined'};
set_content_type(ContentType, Media) ->
    [Type, Subtype] = binary:split(ContentType, <<"/">>),
    Media#wh_media{content_type=Type, content_subtype=Subtype}.

-spec set_content_type(api_binary(), api_binary(), wh_media()) -> wh_media().
set_content_type('undefined', _, Media) -> set_content_type('undefined', Media);
set_content_type(_, 'undefined', Media) -> set_content_type('undefined', Media);
set_content_type(Type, SubType, Media) ->
    Media#wh_media{content_type=Type
                   ,content_subtype=SubType}.

-spec content_length(wh_media()) -> 'undefined' | non_neg_integer().
content_length(#wh_media{content_length=ContentLength}) -> ContentLength.

-spec set_content_length('undefined' | non_neg_integer(), wh_media()) -> wh_media().
set_content_length(ContentLength, Media) ->
    Media#wh_media{content_length=ContentLength}.

-spec media_name(wh_media()) -> api_binary().
media_name(#wh_media{media_name=MediaName}) -> MediaName.

-spec set_media_name(ne_binary(), wh_media()) -> wh_media().
set_media_name(MediaName, Media) ->
    Media#wh_media{media_name=MediaName}.

-spec filename(wh_media()) -> ne_binary().
filename(#wh_media{filename='undefined'
                   ,media_name=MediaName}) ->
    MediaName;
filename(#wh_media{filename=Filename}) -> Filename.

-spec set_filename(ne_binary(), wh_media()) -> wh_media().
set_filename(Filename, Media) ->
    Media#wh_media{filename=Filename}.

-spec metadata(wh_media()) -> wh_json:object().
metadata(#wh_media{metadata=Metadata}) -> Metadata.

-spec set_metadata(wh_json:object(), wh_media()) -> wh_media().
set_metadata(JObj, Media) ->
    Media#wh_media{metadata=JObj}.

-spec merge_metadata(wh_json:object(), wh_media()) -> wh_media().
merge_metadata(JObj, #wh_media{metadata=Metadata}=Media) ->
    PrivJObj = wh_json:private_fields(Metadata),
    Media#wh_media{metadata=wh_json:merge_jobjs(PrivJObj, JObj)}.

-spec content(wh_media()) -> binary() | {'error', _}.
content(#wh_media{engine=Engine, content='undefined'}=Media) ->
    Engine:content(Media);
content(#wh_media{content=Content}) -> Content.

-spec set_content(binary(), wh_media()) -> wh_media().
set_content(Content, Media) ->
    Media#wh_media{content=Content}.

-spec update(wh_media()) -> wh_media().
update(#wh_media{account_db=AccountDb}=Media) ->
    case couch_mgr:save_doc(AccountDb, prepare_metadata(Media)) of
        {'error', _}=Error -> Error;
        {'ok', JObj} ->
            Media#wh_media{metadata_id=wh_json:get_value(<<"_id">>, JObj)
                           ,metadata=JObj}
    end.

prepare_store(Media) -> Media.
     

-spec store(wh_media()) -> wh_media().    
store(#wh_media{engine=Engine}=Media) ->
    Engine:store(Media).

-spec store_url(wh_media()) -> ne_binary().
store_url(Media) ->
    %% TODO: verify that metadata is present...
    <<>>.

-spec delete_content(wh_media()) -> wh_media() | {'error', _}.
delete_content(#wh_media{engine=Engine}=Media) ->
    Engine:delete_content(Media).
    
-spec delete(wh_media()) -> 'ok' | {'error', _}.
delete(#wh_media{engine=Engine}=Media) ->
    case Engine:delete(Media) of
        {'error', _}=Error -> Error;
        'ok' -> delete_metadata(Media)
    end.

-spec delete_metadata(wh_media()) -> 'ok' | {'error', _}.
delete_metadata(Media) ->
    AccountDb = wh_media:account_db(Media),
    MetadataId = wh_media:metadata_id(Media),
    case couch_mgr:del_doc(AccountDb, MetadataId) of
        {'error', _}=Error -> Error;        
        {'ok', _} -> 'ok'
    end.

-spec load_metadata(wh_media()) -> {'ok', wh_json:object()} | {'error', _}.
load_metadata(#wh_media{account_db=AccountDb, metadata_id=Id}) ->
    couch_mgr:open_cache_doc(AccountDb, Id).

-spec get_engine(wh_json:object()) -> atom().
get_engine(JObj) -> 'wh_media_bigcouch'.

-spec prepare_metadata(wh_media()) -> wh_json:object().
prepare_metadata(#wh_media{metadata=JObj}) ->
    Props = [{<<"pvt_modified">>, wh_util:current_tstamp()}],
    wh_json:set_values(Props, JObj).

