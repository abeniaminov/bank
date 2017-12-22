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


not_found(_Req) ->
    #{<<"status">> => action_not_found}.