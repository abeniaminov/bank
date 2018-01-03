%%%-------------------------------------------------------------------
%%% @author abeniaminov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Дек. 2017 12:58
%%%-------------------------------------------------------------------
-module(create).
-author("abeniaminov").

-include("bank.hrl").
-include("logger.hrl").

%% API
-export([known_bank/1, bank/0, transfer_params/1, customers/0]).

known_bank(#{bank_id := BankId, name := Name, host := Host, port := Port}) ->
    KnownBank = #known_bank{bank_id = BankId, name = Name, host = Host, port = Port},
    {atomic, ok} = mnesia:transaction(
        fun() ->
            mnesia:write(KnownBank)
        end),
    ok.

transfer_params(#{type := Type, limit := Limit, commission := C}) ->
    TType = #transfer_type{type = Type, limit = Limit, commission = C},
    {atomic, ok} = mnesia:transaction(
        fun() ->
            mnesia:write(TType)
        end),
    ok.

bank() ->
    {ok, BankName} = application:get_env(bank, name),
    {ok, BankId} = application:get_env(bank, bank_no),
    AccountId = utl:to_binary(utl:uuid()),
    Bank = #bank{id = BankId, name = BankName, account_id = AccountId},
    BankAccount = #account{cid = BankId, id = AccountId},
    {atomic, ok} = mnesia:transaction(
        fun() ->
            mnesia:write(Bank),
            mnesia:write(BankAccount)
        end),
    ok.

customers() ->
    case query:get_bank_id() of
        100 ->
            create_customer("Jose", 1, 50000);
        200 ->
            create_customer("Antonio", 1, 50000),
            create_customer("Maria", 2, 1000)
    end,
    ok.

create_customer(Name, Id, Amount) when is_list(Name), is_integer(Id) ->
    BankId = query:get_bank_id(),
    AccountId = utl:to_binary(utl:uuid()),
    Account = #account{id = AccountId, cid = Id},
    Customer = #customer{id = Id, name = Name},
    CardNo = lists:flatten(io_lib:format("~-3.3.0w", [BankId]) ++ io_lib:format("~3.3.0w", [Id])),
    Card = #card{card_no = CardNo, account_id = AccountId, bank_id = BankId, pin_code_hash = utl:md5hex("1111")},
    InitTransaction =
        #transaction{type = refill, transaction_order_id = utl:to_binary(utl:uuid()), id = utl:to_binary(utl:uuid()), type_val = 1, state = ?committed, amount = Amount, account_id = AccountId},
    {atomic, ok} = mnesia:transaction(
        fun() ->
            mnesia:write(Account),
            mnesia:write(Customer),
            mnesia:write(Card),
            mnesia:write(InitTransaction)
        end),
    ok.
