-module(utl).
-compile([export_all]).

-include("logger.hrl").
-include("xml.hrl").



-spec uuid() -> string().
uuid() ->
    R1 = rand:uniform_s(1, round(math:pow(2, 48))) - 1,
    R2 = rand:uniform_s(1, round(math:pow(2, 12))) - 1,
    R3 = rand:uniform_s(1, round(math:pow(2, 32))) - 1,
    R4 = rand:uniform_s(1, round(math:pow(2, 30))) - 1,
    <<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>> = <<R1:48, 4:4, R2:12, 2:2, R3:32, R4:30>>,
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b",
                                    [TL, TM, THV, CSR, CSL, N])).

                                                % now() to seconds + microseconds
utime() ->
    {_M,S,Mi} = erlang:timestamp(),
                                                %    {_M,S,Mi} = os:timestamp(),
    S*1000000+Mi.


udelta({M1, S1, Mi1}, {M2, S2, Mi2}) ->
    (M1 - M2) * 1000000000000 + (S1 - S2) * 1000000 + (Mi1 - Mi2).
%% -----------------------------------------------------------------------------
ugs() ->
    calendar:datetime_to_gregorian_seconds(erlang:universaltime()).

%% --------------------------------------------------------------------
make_ets(Name) -> make_ets(Name, []).

make_ets(Name, Opts) ->
    case ets:info(Name) of
        undefined -> ets:new(Name, [set, public, named_table|Opts]);
        _         -> Name
    end.

drop_ets(Name) ->
    case ets:info(Name) of
        undefined -> ok;
        _         -> ets:delete(Name)
    end.


%% --------------------------------------------------------------------
get_app_param(Key, Default) ->
    case application:get_env(Key) of
        {ok,Val} -> Val;
        _        -> Default
    end.

%% -----------------------------------------------------------------------------
pick(Key, List, Default) ->
    case lists:keysearch(Key, 1, List) of
        false when Default == must ->
            {error, badarg};
        false ->
            Default;
        {value, {Key, Value}} ->
            Value;
        {value, BadArg} ->
            {error, {bad_type, BadArg}}
    end.

%% -----------------------------------------------------------------------------
s2i(Name, S) ->
    case catch(list_to_integer(S)) of
        {'EXIT', _Why} ->
            throw({error,{bad_int_str, {Name,S}}});
        Val -> Val
    end.

                                                % -----------------------------------------------------------------------------
get_param(Key, Params, Default) ->
    case pick(Key, Params, []) of
        []  -> Default;
        Val -> Val
    end.

                                                % -----------------------------------------------------------------------------
swap([], Acc) -> lists:reverse(Acc);
swap([B1,B2|Tail], Acc) ->
    swap(Tail, [B1,B2|Acc]);
swap(_, Acc) -> swap([], Acc).


%% --------------------------------------------------------------------
whereis_phone(Phone) when is_atom(Phone) ->
    whereis(Phone);
whereis_phone(Phone) when is_list(Phone) ->
    whereis(to_atom(Phone));
whereis_phone(Phone) when is_integer(Phone) ->
    whereis_phone(integer_to_list(Phone)).

%% --------------------------------------------------------------------
to_integer(V) when is_list(V) ->
    list_to_integer(V);
to_integer(V) ->
    V.

%% --------------------------------------------------------------------
to_atom(Val) when is_atom(Val) -> Val;
to_atom(Val) when is_list(Val) ->
    case catch(erlang:list_to_existing_atom(Val)) of
        {'EXIT', _} -> erlang:list_to_atom(Val);
        Atom -> Atom
    end;
to_atom(Val) when is_integer(Val) ->
    to_atom(integer_to_list(Val));
to_atom(Val) when is_binary(Val) ->
    to_atom(binary_to_list(Val)).


to_int(Val) when is_integer(Val) -> Val;
to_int(Val) when is_list(Val) ->
    case catch(erlang:list_to_integer(Val)) of
        {'EXIT', _} -> throw({error, {to_int, bad_arg, Val}});
        Int -> Int
    end;
to_int(Val) when is_atom(Val) ->
    to_int(atom_to_list(Val));
to_int(Val) when is_binary(Val) ->
    case catch(list_to_integer(binary_to_list(Val))) of
        {'EXIT', _} -> throw({error, {to_int, bad_arg, Val}});
        Int -> Int
    end.



to_list(Val) when is_list(Val) -> Val;
to_list(Val) when is_atom(Val) ->
    atom_to_list(Val);
to_list(Val) when is_integer(Val) ->
    integer_to_list(Val);
to_list(Val) when is_float(Val) ->
    float_to_list(Val);
to_list(Val) when is_binary(Val) ->
    binary_to_list(Val).


to_float(Val) when is_list(Val) ->
    case catch(erlang:list_to_float(Val)) of
        {'EXIT', _} ->
            case catch(erlang:list_to_float(Val ++ ".0")) of
                {'EXIT', _} ->

                    case catch(erlang:list_to_integer(Val)) of
                        {'EXIT', _} -> throw({error, {to_float, bad_arg, Val}});
                        Int -> Int
                    end;
                Float2 -> Float2
            end;
        Float -> Float
    end;
to_float(Val) when is_integer(Val) ->
    to_float(integer_to_list(Val));
to_float(Val) when is_float(Val) ->
    Val.


                                                % -----------------------------------------------------------------------------

get_config_name() ->
    case init:get_argument(config) of
        error -> throw({error,{config,undefined}});
        {ok,[[File]]} -> File ++ ".config"
    end.


get_app_params(App, File) ->
    case file:consult(File) of
        {ok, [L]} ->
            proplists:get_value(App, L, []);
        Err -> throw(Err)
    end.

notify_procs(Msg, L) ->
    F = fun({Type,Proc}) ->
                case whereis(Proc) of
                    Pid when is_pid(Pid) -> notify_proc(Type, Pid, Msg);
                    _ -> ok
                end
        end,
    lists:foreach(F, L).

notify_proc(gen_server, Pid, Msg) ->
    gen_server:cast(Pid, Msg);
notify_proc(gen_event, Pid, Msg) ->
    gen_event:notify(Pid, Msg).



unixtime_to_localDatetime(T) ->
    X = T div 1000,
    calendar:now_to_local_time({X div 1000000,X rem 1000000,0}).

unixtime_to_universalDatetime(T) ->
    X = T div 1000,
    calendar:now_to_universal_time({X div 1000000,X rem 1000000,0}).

to_hex(Val) when is_list(Val) ->
    lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- Val]);
to_hex(Val) when is_binary(Val) ->
    to_hex(binary_to_list(Val)).


hex_to_binary(Val) ->
    list_to_binary(hex_to_list(Val)).


hex_to_list([X1, X2 | T], Res) ->
    hex_to_list(T, [erlang:list_to_integer([X1,X2], 16) | Res]);
hex_to_list([], Res) ->
    lists:reverse(Res).

hex_to_list(Val) ->
    hex_to_list(Val, []).
                                                %    [erlang:list_to_integer(X, 16) || X <- Val].

setPlistVal([H|T], Plist) ->
    setPlistVal(T, setPlistVal(H, Plist));
setPlistVal([], Plist) ->
    Plist;
setPlistVal({Key, Val}, Plist) ->
    [{Key, Val} | proplists:delete(Key, Plist)].



datafmt_dmd({{YY, MM, DD}, {HH, M, SS}}) ->
    ?FMT("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [YY, MM, DD, HH, M, SS]).


trigram(Word) when length(Word) < 3->
    {error, length};
trigram(Word) ->
    {ok, lists:reverse(trigram(Word, []))}.


trigram([A,B,C|T], [])->
    trigram([A,B,C|T], [[$_, A, B], [$_, $_, A]]);
trigram([A,B], Ret)->
    [[B, $_, $_], [A,B, $_]|Ret];
trigram([A,B,C|T], Ret) ->
    trigram([B,C|T], [[A,B,C]|Ret]).

getPhoneNumber(Str) ->
    Str1 = string:strip(utl:to_list(Str)),
    RegEx = "^((8|\\+7)[\\- ]?)?(\\(?\\d{3}\\)?[\\- ]?)?[\\d\\- ]{7,10}$",
    case re:run(utl:to_list(Str1), RegEx,[{capture,all_but_first,list}]) of
        nomatch ->
            nomatch;
        {match, _ } ->
            Str2 = case string:substr(Str1, 1, 2) of
                       "+7" -> string:substr(Str1, 2);
                       [$8|_]-> "7" ++ string:substr(Str1, 2);
                       _Other -> Str1
                   end,
            lists:filter(fun(X) -> lists:member(X, "1234567890") end,
                         Str2)
    end.

-spec(md5hex(string()) -> string()).
md5hex(S) ->
    string:to_upper(lists:flatten([[integer_to_list(N, 16) || <<N:4>> <= erlang:md5(S)]])).

formatDatetime({Dt, Tm}, Z) ->
                                                %datafmt_dmd({{YY, MM, DD}, {HH, M, SS}}) ->
                                                %    ?FMT("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [YY, MM, DD, HH, M, SS]).
    ?FMT("~s ~s ~s", [formatDate(Dt), formatTimeShort(Tm), Z]).

formatDate({Y, M, D}) ->
    ?FMT("~p-~p-~p", [Y, M, D]).

formatTimeShort({H, M, _}) ->
    ?FMT("~p:~p", [H, M]).

arplookup({IP1,IP2,IP3,IP4}) ->

    {ok, FD} = file:open("/proc/net/arp", [read,raw]),
    arploop(FD, inet_parse:ntoa({IP1,IP2,IP3,IP4})).


-define(HWADDR_OFF, 4).

arploop(FD, Address) ->
    case file:read_line(FD) of
        eof ->
            file:close(FD),
            not_found;
        {ok, Line} ->
            case lists:prefix(Address, Line) of
                true ->
                    file:close(FD),
                    M = string:tokens(lists:nth(?HWADDR_OFF, string:tokens(Line, " \n")), ":"),
                    list_to_tuple([ erlang:list_to_integer(E, 16) || E <- M ]);
                false -> arploop(FD, Address)
            end
    end.


generate_rand(PassLen) ->

    {_A1, A2, A3} = erlang:timestamp(),
    rand:seed(A2, A3),
                                                %    L = "DFGJLQRSVWZ23456789",
    L = "0123456789",

    [lists:nth(rand:uniform_s(length(L)), L) || _ <- lists:seq(1, PassLen)].


to_binary(A) when is_atom(A) ->
    to_binary(atom_to_list(A));
to_binary(B) when is_binary(B) -> B;
to_binary(I) when is_integer(I) -> to_binary(integer_to_list(I));
to_binary(F) when is_float(F) -> to_binary(nitro_mochinum:digits(F));
to_binary(L) when is_list(L) -> list_to_binary(L).

msToDateTime(Milliseconds) ->

    BaseDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    Seconds       = BaseDate + (Milliseconds div 1000),
    calendar:gregorian_seconds_to_datetime(Seconds).



formatTimeLeftSec(TimeLeftSec) ->
    TT = calendar:seconds_to_time(TimeLeftSec rem 86400),
    DD = TimeLeftSec div 86400,
    ?FMT("~p days, ~s ", [DD, utl:formatTimeShort(TT)]).

getNowGS() ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).




strip_time({{Y,M, D},{_,_,_}}) -> {{Y,M,D}, {0,0,0}}.

add_seconds_to_date(Seconds, Date) ->
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Date) + Seconds).

f_val([]) -> null;
f_val([[]]) -> null;
f_val([[<<>>]]) -> null;
f_val([<<>>]) -> null;
f_val(<<>>) -> null;
f_val(<<"undefined">>) -> null;
f_val([[undefined]]) -> null;
f_val(null) -> null;
f_val(undefined) -> null;
f_val(V) -> V.

f_v_to_list(V) ->
    case f_val(V) of
        null ->
            null;
        V -> to_list(V)
    end.

is_val(V) -> f_val(V) =/= null.

maybe_val(null, Default) -> Default;
maybe_val(undefined, Default) -> Default;
maybe_val(T,_Def) -> T.

iif_null(null, Default, _V ) -> Default;
iif_null(undefined, Default, _V) -> Default;
iif_null(_, _Def, V) -> V.

iif(true, T, _F) -> T;
iif(false, _T, F) -> F.

get_env(App, Branch, Key, Default) ->
    {ok, Config} = application:get_env(App, Branch),
    proplists:get_value(Key, Config, Default).

int_to_bool(I) when is_integer(I), I =< 0 -> false;
int_to_bool(I) when is_integer(I), I > 0 -> true.

prolog() -> ?xml_prolog.

profile(M,F, A) ->
    {Time, Val} = timer:tc(M,F,A),
    io:format("{=====Profile== ~p msec }: ~p:~p~p~n", [Time div 1000, M, F, A]),
    Val.
date_str({date, {Y,M,D}}) ->
    list_to_binary(?FMT("~w-~.2.0w-~.2.0w", [Y,M,D]));
date_str({Y,M,D}) ->
    list_to_binary(?FMT("~w-~.2.0w-~.2.0w", [Y,M,D])).

sign(Str) ->
    string:to_lower(lists:flatten([[integer_to_list(N, 16) || <<N:4>> <= crypto:hash(sha256, to_binary(Str))]])).

sign(Str, Method) ->
    string:to_lower(lists:flatten([[integer_to_list(N, 16) || <<N:4>> <= crypto:hash(Method, to_binary(Str))]])).

fill_before(Str, Char, ToLength) ->
    case length(Str) of
        Length when Length >= ToLength -> Str;
        _ -> fill_before([Char | Str], Char, ToLength)
    end.

