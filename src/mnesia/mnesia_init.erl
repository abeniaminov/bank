%%%-------------------------------------------------------------------
%%% @author abeniaminov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Дек. 2017 11:24
%%%-------------------------------------------------------------------
-module(mnesia_init).
-author("abeniaminov").

-include("bank.hrl").
-include("logger.hrl").

%% API
-export([database_init/0]).

database_init() ->
    mnesia:create_table(known_bank,        [{attributes, record_info(fields, known_bank)}]),
    mnesia:create_table(session,           [{attributes, record_info(fields, session)}]),
    mnesia:create_table(customer,          [{attributes, record_info(fields, customer)}]),
    mnesia:create_table(account,           [{attributes, record_info(fields, account)}]),
    mnesia:create_table(bank,              [{attributes, record_info(fields, bank)}]),
    mnesia:create_table(transaction_order, [{attributes, record_info(fields, transaction_order)}]),
    mnesia:create_table(transaction,       [{attributes, record_info(fields, transaction)}]),
    mnesia:create_table(card,              [{attributes, record_info(fields, card)}]),
    mnesia:create_table(card_order,        [{attributes, record_info(fields, card_order)}]),
    mnesia:create_table(transfer_type,     [{attributes, record_info(fields, transfer_type)}]),
    ok = init_tables().




init_tables() ->
    {ok, KnownBanks} = application:get_env(known_bank),

    lists:foreach(fun({_, X}) -> create:known_bank(maps:from_list(X)) end, KnownBanks),

    {ok, TParams} = application:get_env(transfer_params),
    ?DEBUG_PRINT("TParams", TParams, ?LINE ),
    lists:foreach(fun({_, X}) -> create:transfer_params(maps:from_list(X)) end, TParams),


    ok = create:bank(),
    ok = create:customers(),


    ok.

