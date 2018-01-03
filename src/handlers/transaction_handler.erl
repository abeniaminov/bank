%%%-------------------------------------------------------------------
%%% @author abeniaminov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Дек. 2017 19:49
%%%-------------------------------------------------------------------
-module(transaction_handler).
-author("abeniaminov").

-include("logger.hrl").
-include("bank.hrl").
%% API
-export([init/2]).
-export([content_types_provided/2]).

-export([resp_to_json/2, prepare/1, rollback/1, commit/1]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, resp_to_json}
    ], Req, State}.



resp_to_json(#{path := Path} = Req, State) ->
    Action = utl:to_atom(hd(lists:reverse(string:tokens(binary_to_list(Path), "/")))),
    MapResult =
        case catch(?MODULE:Action(Req)) of
            {'EXIT',R} ->
                not_found(Req);

            Map -> Map
        end,
    Body =
        jsx:encode(
            #{
                <<"result">> => MapResult
            }
        ),
    {Body, Req, State}.



prepare(Req) ->
     Qs =
        try cowboy_req:match_qs(
                    [
                        {card_no, nonempty},
                        {transfer_order_id, nonempty},
                        {operation, nonempty},
                        {type, nonempty},
                        {amount, nonempty}
                    ], Req)
        catch _:_ ->
            throw({error, bad_parameter})
        end,
    i_prepare(Qs).

rollback(Req) ->
    Qs =
        try cowboy_req:match_qs(
            [
                {transaction_order_id, nonempty}
            ], Req)
        catch _:_ ->
            throw({error, bad_parameter})
        end,
    i_rollback(Qs).

commit(Req) ->
    Qs =
        try cowboy_req:match_qs(
            [
                {transaction_order_id, nonempty}
            ], Req)
        catch _:_ ->
            throw({error, bad_parameter})
        end,
    i_commit(Qs).



not_found(_Req) ->
    #{<<"status">> => action_not_found}.


i_prepare(#{card_no := CardNo,  transfer_order_id := ToId, operation := Op,  type := Type, amount := Amount} = Qs) ->
    case mnesia:transaction(fun() ->
         AmountF = utl:to_float(utl:to_list(Amount)),
         AccId = query:get_account_by_cardno(utl:to_list(CardNo)),
         {Commission, Limit} = query:get_transfer_params(utl:to_atom(Type)),
         CommissionAmount = list_to_float(float_to_list(AmountF*Commission/100, [{decimals, 2}])),
        ?DEBUG_PRINT("CommissionAmount", CommissionAmount, ?LINE),
         case Limit < 0 of
             true -> continue;
             false ->
                 case Limit >= AmountF of
                     true -> continue;
                     false -> mnesia:abort(?FMTB("Transaction limit is exceeded", []))
                 end
         end,
         AccAmount = query:get_account_amount(AccId),
        case utl:to_atom(Op) of
            withdraw ->
                case  AccAmount > AmountF + CommissionAmount of
                    true -> continue;
                    false -> mnesia:abort(?FMTB("Insufficient funds", []))
                end;
            refill -> continue
        end,

       {ok, TransactionOrderId} = query:create_transaction_order(Qs#{account_id => AccId, commission => Commission, limit => Limit }),
            #{<<"transaction_order_id">> => TransactionOrderId, <<"commission">> => Commission, <<"limit">> => Limit}
        end) of
        {atomic, Res} ->
            maps:merge(
                #{
                    <<"status">> => ok,
                    <<"state">> => prepared,
                    <<"reason">> => null,
                    <<"transfer_order_id">> => ToId}, Res);
        {aborted, Reason} ->
                #{
                    <<"status">> => ok,
                    <<"state">> => aborted,
                    <<"reason">> =>  Reason,
                    <<"transfer_order_id">> => ToId}
    end.


i_rollback(#{transaction_order_id := ToId} = Qs) ->
    case mnesia:transaction(fun() ->
        TrOrder = query:get_transaction_order(ToId),
        case TrOrder of
            not_found ->
                rollbacked;
            T when is_record(T, transaction_order) ->
                query:rollback_transactions(TrOrder#transaction_order.id),
                mnesia:write(TrOrder#transaction_order{state = ?rollbacked}),
                rollbacked
        end
                            end) of
        {atomic, Res} ->
            #{
                <<"status">> => ok,
                <<"state">> => Res,
                <<"reason">> => null,
                <<"transfer_order_id">> => ToId};
        {aborted, Reason} ->
            #{
                <<"status">> => ok,
                <<"state">> => aborted,
                <<"reason">> =>  Reason,
                <<"transfer_order_id">> => ToId}
    end.

i_commit(#{transaction_order_id := ToId} = Qs) ->
    ?DEBUG_PRINT("transaction_order_id", ToId, ?LINE),
    case mnesia:transaction(fun() ->
        TrOrder = query:get_transaction_order(ToId),
        ?DEBUG_PRINT("TrOrder", TrOrder, ?LINE),
        case TrOrder of
            not_found ->
                committed;
            T when is_record(T, transaction_order) ->
                query:commit_transactions(TrOrder#transaction_order.id),
                mnesia:write(TrOrder#transaction_order{state = ?committed}),
                committed
        end
                            end) of
        {atomic, Res} ->
            #{
                <<"status">> => ok,
                <<"state">> => Res,
                <<"reason">> => null,
                <<"transfer_order_id">> => ToId};
        {aborted, Reason} ->
            #{
                <<"status">> => ok,
                <<"state">> => aborted,
                <<"reason">> =>  Reason,
                <<"transfer_order_id">> => ToId}
    end.