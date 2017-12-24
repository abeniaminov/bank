%%%-------------------------------------------------------------------
%%% @author abeniaminov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Дек. 2017 14:32
%%%-------------------------------------------------------------------
-module(query).
-author("abeniaminov").

-include("bank.hrl").
-include("logger.hrl").

%% API
-compile([export_all]).

get_object(Q) ->
    mnesia:transaction(fun() ->
                    mnesia:match_object(Q) end).


get_known_bank(Id) ->
    {atomic, L} = mnesia:transaction(fun() ->
        mnesia:read(known_bank, Id) end).

get_bank_id() ->
    {atomic, L} = mnesia:transaction(fun() ->
        mnesia:all_keys(bank) end),
    case length(L) of
        0 -> not_found;
        _ -> hd(L)
    end.

get_account_by_cardno(CardNo) ->
    RCard = mnesia:read(card, CardNo),
    case length(RCard) of
        0 ->
            mnesia:abort(?FMTB("card_no: ~s  not found", [CardNo]));
        1 ->
            RC = hd(RCard),
            RC#card.account_id
    end.

get_transfer_params(Type) ->
    TParams = mnesia:read(card, Type),
    case length(TParams) of
        0 ->
            mnesia:abort(?FMTB("type params: ~s  not found", [Type]));
        1 ->
            TP = hd(TParams),
            {TP#transfer_type.commission, TP#transfer_type.limit}
    end.

create_transaction_order(Qs) ->
    #{type := Type, operation := Op, account_id := AccId, commission := Commission, amount := Amount,
        transfer_ordre_id := ToId} = Qs,
    TROrderId = utl:to_binary(utl:uuid()),
    BankAccount = get_bank_account(),
    Now = erlang:localtime(),
    TransactionOrder =
        #transaction_order{id = TROrderId, type = Type, tr_type = Op, transfer_order_id = ToId,
            account_id = AccId, modified = Now, created = Now , state = ?prepared},
    BaseTransaction =
        #transaction{
            id = utl:to_binary(utl:uuid()),
            transaction_order_id = TROrderId,
            type = Type, type_val = type_val(Type),
            account_id = AccId, amount = Amount,
            state = ?prepared},
    CommissionIncome =
        #transaction{
            id = utl:to_binary(utl:uuid()),
            transaction_order_id = TROrderId,
            type = commission_income, type_val = type_val(commission_income),
            account_id = BankAccount, amount = Amount*Commission/100,
            state = ?prepared},
    CommissionOutlay =
        #transaction{
            id = utl:to_binary(utl:uuid()),
            transaction_order_id = TROrderId,
            type = commission_outlay, type_val = type_val(commission_outlay),
            account_id = AccId, amount = Amount*Commission/100,
            state = ?prepared},
    mnesia:write(TransactionOrder),
    mnesia:write(BaseTransaction),
    mnesia:write(CommissionIncome),
    mnesia:write(CommissionOutlay),

    {ok, TROrderId}.

get_account_amount(AccountId) ->
    Q = ?q_transaction#transaction{account_id = AccountId, state = ?committed},
    {atomic, L} = mnesia:match_object(Q),
    case length(L) of
        0 -> 0;
        _ -> lists:foldl(fun(X, Acc) -> Acc + X#transaction.amount * X#transaction.type_val end, 0, L)
    end.

type_val(refill) -> 1;
type_val(withdraw) -> -1;
type_val(commission_income) -> 1;
type_val(commission_outlay) -> -1.

get_bank_account() ->
    {atomic, L} = mnesia:all_keys(bank),
    {atomic, B} = mnesia:read(bank, hd(L)),
    B#bank.account_id.

get_transaction_order(ToId) ->
    Q = ?q_transaction#transaction{transaction_order_id = ToId, state = ?prepared},
    {atomic, L} = mnesia:match_object(Q),
    case length(L) of
        0 -> not_found;
        1 -> hd(L)
    end.

rollback_transactions(TrOrderId) ->
    Q = ?q_transaction#transaction{transaction_order_id = TrOrderId},
    {atomic, L} = mnesia:match_object(Q),
    lists:foreach(fun(X) -> mnesia:write(X#transaction{state = ?rollbacked}) end, L).

commit_transactions(TrOrderId) ->
    Q = ?q_transaction#transaction{transaction_order_id = TrOrderId},
    {atomic, L} = mnesia:match_object(Q),
    lists:foreach(fun(X) -> mnesia:write(X#transaction{state = ?committed}) end, L).
