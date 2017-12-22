%%%-------------------------------------------------------------------
%%% @author abeniaminov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Дек. 2017 19:29
%%%-------------------------------------------------------------------
-module(authorisation_handler).
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
            "login" -> login(Req);
            "logou" -> logot(Req);
            _ -> not_found(Req)
        end,
    Body =
        jsx:encode(
            #{
                <<"result">> => MapResult
            }
        ),
    {Body, Req, State}.


login(Req) ->
    #{<<"status">> => ok}.

logout(Req) ->
    #{<<"status">> => ok}.


not_found(_Req) ->
    #{<<"status">> => action_not_found}.