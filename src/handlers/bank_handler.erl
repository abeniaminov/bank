%%%-------------------------------------------------------------------
%%% @author abeniaminov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Дек. 2017 20:22
%%%-------------------------------------------------------------------
-module(bank_handler).
-author("abeniaminov").

-include("logger.hrl").
-include("bank.hrl").

%% API
-export([init/2]).
-export([content_types_provided/2]).

-export([resp_to_json/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, resp_to_json}
    ], Req, State}.



resp_to_json(#{path := Path} = Req, State) ->
    Action = hd(lists:reverse(string:tokens(binary_to_list(Path), "/"))),
    MapResult =
        case Action of
            "create_customer" -> create_customer(Req);
            "get_limits" -> get_limits(Req);
            _ -> not_found(Req)
        end,
    Body =
        jsx:encode(
            #{
                <<"result">> => MapResult
            }
        ),
    {Body, Req, State}.


create_customer(Req) ->
    #{<<"status">> => ok}.

get_limits(Req) ->
    #{type := Type} = Qs =
        try cowboy_req:match_qs(
            [
                {type, nonempty}
            ], Req)
        catch _:_ ->
            throw({error, bad_parameter})
        end,
    {atomic, [V]} =
        mnesia:transaction(fun() ->
            mnesia:read(transfer_type, utl:to_atom(Type))
            end),
    #transfer_type{limit = Limit, commission = Commission} = V,
    #{<<"limit">> => Limit, <<"commission">> => Commission}.



not_found(_Req) ->
    #{<<"status">> => action_not_found}.