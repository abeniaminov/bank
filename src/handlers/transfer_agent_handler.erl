%%%-------------------------------------------------------------------
%%% @author abeniaminov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Дек. 2017 23:21
%%%-------------------------------------------------------------------
-module(transfer_agent_handler).
-author("abeniaminov").


-include("logger.hrl").
-include("bank.hrl").
-include("http_headers.hrl").

%% API
-export([init/2]).
-export([content_types_provided/2]).

-export([resp_to_json/2, order/1]).


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



order(Req) ->
    Qs =
        try cowboy_req:match_qs(
            [
                {card_from, nonempty},
                {card_to, nonempty},
                {operation, nonempty, transfer},
                {amount, nonempty}
            ], Req)
        catch _:_ ->
            throw({error, bad_parameter})
        end,

    i_order(Qs).

not_found(_Req) ->
    #{<<"status">> => action_not_found}.

i_order(#{card_from := CardFrom, card_to := CardTo,  amount := Amount}=Qs) ->
    ?DEBUG_PRINT("QS", Qs, ?LINE),
    BankFromId = get_bank_id_by_card(utl:to_list(CardFrom)),
    ?DEBUG_PRINT("BankFrom", BankFromId, ?LINE),
    BankToId = get_bank_id_by_card(utl:to_list(CardTo)),
    ?DEBUG_PRINT("BankTo", BankToId, ?LINE),
    Type =
        case BankFromId =:= BankToId of
            true -> intra_bank;
            false -> inter_bank
        end,
    #known_bank{host = Host1, port = Port1}  = check_bank(BankFromId, query:get_known_bank(BankFromId)),

    ?DEBUG_PRINT("HOST, PORT", {Host1, Port1}, ?LINE),
    #known_bank{host = Host2, port = Port2} =   check_bank(BankToId, query:get_known_bank(BankToId)),

    {atomic, {card_order, CardOrder}} =
        mnesia:transaction(fun () ->
            query:create_card_order(Qs#{type => Type}) end),



    MParamFrom =
        #{
            card_no => CardFrom,
            amount => Amount,
            type => Type,
            transfer_order_id => CardOrder,
            operation => withdraw,
            host => Host1,
            port => Port1
    },
    #{<<"status">> := <<"ok">>, <<"state">> := State1} =
        send_prepare(MParamFrom),
    #{<<"status">> := <<"ok">>, <<"state">> := State2} =
        send_prepare(MParamFrom#{host => Host2, port => Port2, card_no => CardTo, operation => refill}),
    case State1 == <<"prepared">> andalso State2 == <<"prepared">> of
        true ->
           #{<<"state">> := StateC1} = send_commit(#{host => Host1, port => Port1, transfer_order_id => CardOrder}),
           #{<<"state">> := StateC2} = send_commit(#{host => Host2, port => Port2, transfer_order_id => CardOrder})
        ;
        false ->
            #{<<"state">> := StateR1} = send_rollback(#{host => Host1, port => Port1, transfer_order_id => CardOrder}),
            #{<<"state">> := StateR2} = send_rollback(#{host => Host2, port => Port2, transfer_order_id => CardOrder})
    end.



send_prepare(MParams) ->
    UriPrepare = ?FMT("http://~s:~s~s", [utl:to_list(maps:get(host, MParams)), utl:to_list(maps:get(port, MParams)), "/transaction/prepare"]),
    Params =

            maps:to_list(maps:with([card_no, operation, transfer_order_id, type, amount], MParams))
        ,
    ?DEBUG_PRINT("Params", Params, ?LINE),
    Res = get_maps_by_request(maps:get(host, MParams), maps:get(port, MParams), <<"GET">>, UriPrepare, ?OUTPUT_JSON_HEADERS, Params),
    ?DEBUG_PRINT("RES", Res, ?LINE),
    Res.

send_commit(MParams) ->
    UriPrepare = ?FMT("http://~s:~s~s", [utl:to_list(maps:get(host, MParams)), utl:to_list(maps:get(port, MParams)), "/transaction/commit"]),
    Params =

        maps:to_list(maps:with([transfer_order_id], MParams))
    ,
    ?DEBUG_PRINT("Params", Params, ?LINE),
    Res = get_maps_by_request(maps:get(host, MParams), maps:get(port, MParams), <<"GET">>, UriPrepare, ?OUTPUT_JSON_HEADERS, Params),
    ?DEBUG_PRINT("RES", Res, ?LINE),
    Res.

send_rollback(MParams) ->
    UriPrepare = ?FMT("http://~s:~s~s", [utl:to_list(maps:get(host, MParams)), utl:to_list(maps:get(port, MParams)), "/transaction/rollback"]),
    Params =

        maps:to_list(maps:with([transfer_order_id], MParams))
    ,
    ?DEBUG_PRINT("Params", Params, ?LINE),
    Res = get_maps_by_request(maps:get(host, MParams), maps:get(port, MParams), <<"GET">>, UriPrepare, ?OUTPUT_JSON_HEADERS, Params),
    ?DEBUG_PRINT("RES", Res, ?LINE),
    Res.

check_bank(BankNo, L) when is_list(L), L == []   -> throw({error, ?FMT("unknown_bank ~s~n", [utl:to_list(BankNo)])});
check_bank(_BankNo, [H|T])  ->  H.



get_bank_id_by_card(CardNo) when is_list(CardNo), length(CardNo) == 6 ->
    list_to_integer(CardNo) div 1000;
get_bank_id_by_card(_CardNo) ->
    throw({error, bad_card_no}).


get_maps_by_request(Host, Port, Method, Url, Header, Params) ->
    Url2 = Url ++ "?" ++ lists:join( $& ,[utl:to_list(K) ++ "=" ++ utl:to_list(V) || {K,V} <- Params]),

    Res = httpc:request(Url2),

    case Res of
        {ok, {_, _, JSon}}  ->
            jsx:decode(utl:to_binary(JSon), [return_maps]);
        _ -> throw({error, "Bad answer from Bank"})
    end.