%%%-------------------------------------------------------------------
%%% @author abeniaminov
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Дек. 2017 15:00
%%%-------------------------------------------------------------------
-module(index_handler).
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



resp_to_json(Req, State) ->
    Body =
    jsx:encode(
        #{
            <<"result">> => list_to_binary(bank_app:name())
        }
    ),
    {Body, Req, State}.

