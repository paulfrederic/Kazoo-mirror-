%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kazoo_ips_maintenance).

-include("kazoo_ips.hrl").

-export([refresh/0]).
-export([add/0
         ,add/3
        ]).
-export([assign/0
         ,assign/2
        ]).
-export([release/0
         ,release/1
        ]).
-export([delete/0
         ,delete/1
        ]).
-export([summary/0
         ,summary/1
        ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec refresh() -> 'no_return'.
refresh() ->
    kz_ip_utils:refresh_database(),
    'no_return'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add() -> 'no_return'.
add() ->
    io:format("Please use: sup kazoo_ips_maintenance add <ip> <zone> <host>~n", []),
    'no_return'.

-spec add(ne_binary(), ne_binary(), ne_binary()) -> 'no_return'.
add(IP, Zone, Host) ->
    _ = case kz_ip:create(IP, Zone, Host) of
            {'ok', _} ->
                io:format("added IP ~s to available dedicated ips~n"
                          ,[IP]);
            {'error', _R} ->
                io:format("unable to add IP: ~p~n", [_R])
        end,
    'no_return'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec assign() -> 'no_return'.
assign() ->
    io:format("Please use: sup kazoo_ips_maintenance assign <ip> <account>~n", []),
    'no_return'.

-spec assign(ne_binary(), ne_binary()) -> 'no_return'.
assign(IP, Account) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    AccountId = wh_util:format_account_id(Account, 'raw'),
    _ = case couch_mgr:open_doc(AccountDb, AccountId) of
            {'ok', _} ->
                case kz_ip:assign(Account, IP) of
                    {'ok', _} ->
                        io:format("assigned IP ~s to ~s~n"
                                  ,[IP, Account]);
                    {'error', _R} ->
                        io:format("unable to assign IP: ~p~n", [_R])
                end;
            {'error', _R} ->
                io:format("unable to find account: ~p~n", [_R])
        end,
    'no_return'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec release() -> 'no_return'.
release() ->
    io:format("Please use: sup kazoo_ips_maintenance release <ip>~n", []),
    'no_return'.

-spec release(ne_binary()) -> 'no_return'.
release(IP) ->
    _ = case kz_ip:release(IP) of
            {'ok', _} ->
                io:format("released IP ~s~n", [IP]);
            {'error', _R} ->
                io:format("unable to release IP: ~p~n", [_R])
        end,
    'no_return'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete() -> 'no_return'.
delete() ->
    io:format("Please use: sup kazoo_ips_maintenance delete <ip>~n", []),
    'no_return'.

-spec delete(ne_binary()) -> 'no_return'.
delete(IP) ->
    _ = case kz_ip:delete(IP) of
            {'ok', _} ->
                io:format("deleted IP ~s~n", [IP]);
            {'error', _R} ->
                io:format("unable to delete IP: ~p~n", [_R])
        end,
    'no_return'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec summary() -> 'no_return'.
summary() -> summary('undefined').

-spec summary(api_binary()) -> 'no_return'.
summary(Host) ->
    _ = case kz_ips:summary(Host) of
            {'ok', []} ->
                io:format("No IPs found~n", []);
            {'ok', JObjs} -> print_summary(JObjs);
            {'error', _Reason} ->
                io:format("Unable to list IPs assigned to host ~s: ~p~n"
                          ,[Host, _Reason])
        end,
    'no_return'.

-spec print_summary(wh_json:objects()) -> 'ok'.
print_summary(JObjs) ->
    FormatString = "| ~-15s | ~-10s | ~-10s | ~-30s | ~-32s |~n",
    io:format("+-----------------+------------+------------+--------------------------------+----------------------------------+~n", []),
    print_summary_headers(FormatString),
    io:format("+=================+============+============+================================+==================================+~n", []),
    print_summary_row(JObjs, FormatString),
    io:format("+-----------------+------------+------------+--------------------------------+----------------------------------+~n", []).

-spec print_summary_headers(string()) -> 'ok'.
print_summary_headers(FormatString) ->
    Headers = [<<"IP">>
               ,<<"Status">>
               ,<<"Zone">>
               ,<<"Host">>
               ,<<"Account">>
              ],
    io:format(FormatString, Headers).

-spec print_summary_row(wh_json:objects(), string()) -> 'ok'.
print_summary_row([], _) -> 'ok';
print_summary_row([JObj|JObjs], FormatString) ->
    io:format(FormatString,
              [wh_json:get_value(<<"ip">>, JObj)
               ,wh_json:get_value(<<"status">>, JObj)
               ,wh_json:get_value(<<"zone">>, JObj)
               ,wh_json:get_value(<<"host">>, JObj)
               ,wh_json:get_value(<<"assigned_to">>, JObj)
              ]),
    print_summary_row(JObjs, FormatString).
