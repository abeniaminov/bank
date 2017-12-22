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
    Action = utl:to_atom(hd(lists:reverse(string:tokens(binary_to_list(Path), "/")))),
    MapResult =
        case catch(Action(Req)) of
            Map -> Map;
            {'EXIT',_} -> not_found(Req)
        end,
    Body =
        jsx:encode(
            #{
                <<"result">> => MapResult
            }
        ),
    {Body, Req, State}.



request(Req) ->
    #{<<"status">> => ok}.

pre_commit(Req) ->
    #{<<"status">> => ok}.

rollback(Req) ->
    #{<<"status">> => ok}.

commit(Req) ->
    #{<<"status">> => ok}.



not_found(_Req) ->
    #{<<"status">> => action_not_found}.