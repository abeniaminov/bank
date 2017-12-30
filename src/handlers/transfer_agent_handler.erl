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
    MapResult =
        case catch(?MODULE:Action(Req)) of
            {'EXIT',R} ->
                not_found(Req);

            Map -> Map
        end,

    MapResS =  maps:get(<<"result">>, MapResult, null),
    MapRes =
        case MapResS of
            null -> #{
                <<"result">> => MapResult
                    };
            _ -> MapResult
        end,
    {jsx:encode(MapRes), Req, State}.



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

i_order(#{card_from := CardNo, card_to := CardNo,  amount := _Amount}) ->
    #{<<"status">> => ok, <<"state">> => aborted, <<"reason">> => <<"Duplicate cards">>};
i_order(#{card_from := CardFrom, card_to := CardTo,  amount := Amount}=Qs) ->
    BankFromId = get_bank_id_by_card(utl:to_list(CardFrom)),
    BankToId = get_bank_id_by_card(utl:to_list(CardTo)),
    Type =
        case BankFromId =:= BankToId of
            true -> intra_bank;
            false -> inter_bank
        end,
    #known_bank{host = Host1, port = Port1}  = check_bank(BankFromId, query:get_known_bank(BankFromId)),
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
    Res1 = send_prepare(MParamFrom),
    {ok, State1} =  check_result_status(null, Res1),
    Res2 = send_prepare(MParamFrom#{host => Host2, port => Port2, card_no => CardTo, operation => refill}),
    {ok, State2} =
    case check_result_status(State1, Res2) of
        {ok, S} -> {ok, S} ;
        {error, Status2} ->
            case State1 of
                <<"prepared">> -> send_rollback(#{host => Host1, port => Port1, transfer_order_id => CardOrder});
                _ -> Res2
            end

    end,

    case State1 == <<"prepared">> andalso State2 == <<"prepared">> of
        true ->
            send_commit(#{host => Host1, port => Port1, transfer_order_id => CardOrder}),
            send_commit(#{host => Host2, port => Port2, transfer_order_id => CardOrder});
        false ->
            case State1 of
                <<"prepared">> -> send_rollback(#{host => Host1, port => Port1, transfer_order_id => CardOrder}), Res2;
                _ -> Res1
            end,
            case State2 of
                <<"prepared">> -> send_rollback(#{host => Host2, port => Port2, transfer_order_id => CardOrder}), Res1;
                _ -> Res2
            end
    end.



send_prepare(MParams) ->
    UriPrepare = ?FMT("http://~s:~s~s", [utl:to_list(maps:get(host, MParams)), utl:to_list(maps:get(port, MParams)), "/transaction/prepare"]),
    Params = maps:to_list(maps:with([card_no, operation, transfer_order_id, type, amount], MParams)),
    get_maps_by_request(maps:get(host, MParams), maps:get(port, MParams), <<"GET">>, UriPrepare, ?OUTPUT_JSON_HEADERS, Params).

send_commit(MParams) ->
    UriPrepare = ?FMT("http://~s:~s~s", [utl:to_list(maps:get(host, MParams)), utl:to_list(maps:get(port, MParams)), "/transaction/commit"]),
    Params = maps:to_list(maps:with([transfer_order_id], MParams)),
    get_maps_by_request(maps:get(host, MParams), maps:get(port, MParams), <<"GET">>, UriPrepare, ?OUTPUT_JSON_HEADERS, Params).

send_rollback(MParams) ->
    UriPrepare = ?FMT("http://~s:~s~s", [utl:to_list(maps:get(host, MParams)), utl:to_list(maps:get(port, MParams)), "/transaction/rollback"]),
    Params = maps:to_list(maps:with([transfer_order_id], MParams)),
    get_maps_by_request(maps:get(host, MParams), maps:get(port, MParams), <<"GET">>, UriPrepare, ?OUTPUT_JSON_HEADERS, Params).

check_bank(BankNo, L) when is_list(L), L == []   -> throw({error, ?FMT("unknown_bank ~s~n", [utl:to_list(BankNo)])});
check_bank(_BankNo, [H|T])  ->  H.



get_bank_id_by_card(CardNo) when is_list(CardNo), length(CardNo) == 6 ->
    list_to_integer(CardNo) div 1000;
get_bank_id_by_card(_CardNo) ->
    throw({error, bad_card_no}).


get_maps_by_request(_Host, _Port, _Method, Url, _Header, Params) ->
    Url2 = Url ++ "?" ++ lists:join( $& ,[utl:to_list(K) ++ "=" ++ utl:to_list(V) || {K,V} <- Params]),

    Res = httpc:request(Url2),

    case Res of
        {ok, {_, _, JSon}}  ->
            jsx:decode(utl:to_binary(JSon), [return_maps]);
        _ -> throw({error, "Bad answer from Bank"})
    end.

check_result_status(PState, #{<<"result">> := Res}) ->
    Status = maps:get(<<"status">>, Res),
    case Status of
        <<"ok">> -> {ok, maps:get(<<"state">>, Res)};
        Other ->
            case PState of
                null -> throw({error, Other});
                _ -> {error, Other}
            end
    end.