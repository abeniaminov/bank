%%%-------------------------------------------------------------------
%% @doc bank public API
%% @end
%%%-------------------------------------------------------------------

-module(bank_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1, name/0]).

%%====================================================================
%% API
%%====================================================================

start(_Type, _Args) ->
    {ok, Pid} =  bank_sup:start_link(),
    ok = start_cowboy(),
    {ok, Pid}.
    
start() ->
    inets:start(),
    ok = application:start(crypto),
    ok = application:start(asn1),
    ok = application:start(public_key),
    ok = application:start(ssl),

    ok = application:start(mnesia),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    ok = application:start(bank).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

-spec name() -> iolist().
name() ->
    {ok, Version} = application:get_key(bank, vsn),
    io_lib:format("Bank API server version ~s", [Version]).


-spec start_cowboy() -> ok.
start_cowboy() ->
    case application:get_env(cowboy) of
        {ok, Config} when erlang:is_list(Config) ->
            NumAcceptors = proplists:get_value(nbacceptors, Config),
            IP = proplists:get_value(ip, Config),
            Port = proplists:get_value(http_port, Config),
            HttpParams = [{ip, IP}, {port, Port}, {num_acceptors, NumAcceptors}],

            Dispatch = cowboy_router:compile(routes()),
            {ok, _} = cowboy:start_clear(http, HttpParams, #{
                env => #{dispatch => Dispatch}
            }),
            ok;
        undefined -> throw("COWBOY SETTINGS NOT FOUND, CHECK YOU CONFIG")
    end.

-spec routes() -> list().
routes()->
    [{'_',
            lists:map(fun({U, H}) -> {U, H, []} end, api())
            ++ [{'_', not_found_handler, []}]}
    ].


-spec api() -> [{string(), atom()}].
api() ->
    [
        {"/", index_handler},
        {"/request/transfer", request_hanler},
        {"/request/get_trasfer_history", request_hanler}
        ].