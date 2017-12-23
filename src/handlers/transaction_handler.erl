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
    ?DEBUG_PRINT("Action", Action, ?LINE),
    MapResult =
        case catch(?MODULE:Action(Req)) of
            {'EXIT',R} ->
                ?DEBUG_PRINT("ERROR Action", R, ?LINE),
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
    #{<<"status">> => rollback}.

commit(Req) ->
    #{<<"status">> => ok}.



not_found(_Req) ->
    #{<<"status">> => action_not_found}.


i_prepare(#{card_no := CardNo,  transfer_order_id := ToId, type := Type, amount := Amount} = Qs) ->
    case mnesia:transaction(fun() ->
            AccId = query:get_account_by_cardno(CardNo),
            {Commission, Limit} = query:get_transfer_params(Type),
            case Limit < 0 of
                true -> continue;
                false ->
                    case Limit >= Amount of
                        true -> continue;
                        false -> mnesia:abort(?FMTB("Transaction limit is exceeded", []))
                    end
            end,
            AccAmount = query:get_account_amount(AccId),
            case  AccAmount > Amount* (1 + Commission/100) of
                true -> continue;
                false -> mnesia:abort(?FMTB("Insufficient funds", []))
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
                #{<<"status">> => ok,
                  <<"state">> => prepared,
                  <<"reason">> =>  Reason,
                  <<"transfer_order_id">> => ToId}
    end.

