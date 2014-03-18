%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(kz_utils).

-export([get_configuration/2]).

-include("knapsack.hrl").


get_engine(Configuration, Type) ->
    'ok'.
    

get_configuration(Account, AdditionalIds) ->
    AccountDb = wh_utl:format_account_id(Account, 'encoded'),
    AccountId = wh_utl:format_account_id(Account, 'raw'),
    get_configuration(AccountDb, [AccountId|AdditionalIds], wh_json:new()).

get_configuration(_, [], Configuration) -> Configuration;
get_configuration(AccountDb, ['undefined'|Ids], Configuration) ->
    get_configuration(AccountDb, Ids, Configuration);
get_configuration(AccountDb, [Id|Ids], Configuration) ->
    case couch_mgr:open_cache_doc(AccountDb, Id) of
        {'ok', JObj} ->
            get_configuration(AccountDb, Ids, merge_configuration(JObj, Configuration));
        {'error', _R} ->
            lager:debug("unable to load document ~s/~s: ~p", [AccountDb, Id, _R]),
            get_configuration(AccountDb, Ids, Configuration)
    end.


merge_configuration(JObj, Configuration) ->
    case wh_json:get_ne_value(<<"knapsack">>, JObj) of
        'undefined' -> Configuration;
         Object -> wh_json:merge_recursive(Configuration, Object)
    end.
