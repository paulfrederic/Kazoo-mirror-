%%%-------------------------------------------------------------------
%%% @copyright (C) 2014 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(wh_media_identifier).

-export([basic/2]).

-include("whistle_media.hrl").

basic(Account, Id) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    {AccountDb, Id}.
