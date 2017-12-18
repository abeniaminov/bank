%%%-------------------------------------------------------------------
%%% @author abeniaminov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Дек. 2017 17:30
%%%-------------------------------------------------------------------
-module(not_found_handler).
-author("abeniaminov").

-export([init/2]).
-export([content_types_provided/2]).

-export([resp_to_json/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, resp_to_json}
    ], Req, State}.



resp_to_json(Req, State) ->
    Body =
        jsx:encode(
            #{
                <<"status">> => action_not_found
            }
        ),
    {Body, Req, State}.