-define(fl(List), lists:flatten(List)).
-define( FMT(F,P), lists:flatten(io_lib:format(F,P)) ).
-define( FMTB(F,P), utl:to_binary(lists:flatten(io_lib:format(F,P))) ).

-define(DEBUG(Format, Data), lager:info("~p (~p): " ++ Format, case Data of undefined ->
                                                                      [?MODULE, ?LINE];
                                                                  _ -> [?MODULE, ?LINE] ++ Data end)
       ).


-define(INFO(Format, Data), lager:info("~p (~p): " ++ Format, case Data of undefined ->
                                                                      [?MODULE, ?LINE];
                                                                  _ -> [?MODULE, ?LINE] ++ Data end)
       ).

-define(WARN(Format, Data), lager:warning("~p (~p): " ++ Format, case Data of undefined ->
                                                                      [?MODULE, ?LINE];
                                                                  _ -> [?MODULE, ?LINE] ++ Data end)
       ).

-define(ERROR(Format, Data), lager:error("~p (~p): " ++ Format, case Data of undefined ->
                                                                      [?MODULE, ?LINE];
                                                                  _ -> [?MODULE, ?LINE] ++ Data end)
       ).


%%-define(debug, true).
-ifdef(debug).
-define(DEBUG_PRINT(X, L),         io:format("{=====DEBUG== ~p,LINE: ~p}: ~p~n", [?MODULE, L ,X])).
-define(DEBUG_PRINT(X, Y, L),      io:format("{=====DEBUG== ~p,LINE: ~p}: ~p ~p~n", [?MODULE, L, X, Y])).
-define(DEBUG_PRINT(X, Y, Z, L),   io:format("{=====DEBUG== ~p,LINE: ~p}: ~p ~p ~p~n", [?MODULE, L, X, Y, Z])).
-define(POFILE_FUN(Module, Fun, Params), utl:profile(Module, Fun, Params)).
-define(PROFILE_PRINT(T1, L),      io:format("{=====Profile== ~p LINE: ~p}: ~p ms, ~n", [?MODULE, L, erlang:monotonic_time(milli_seconds) - T1])).
-else.
-define(DEBUG_PRINT(X, L), true).
-define(DEBUG_PRINT(X, Y, L), true).
-define(DEBUG_PRINT(X, Y, Z, L), true).
-define(POFILE_FUN(Module, Fun, Params), apply(Module, Fun, Params)).
-define(PROFILE_PRINT(T1, L), true).
-endif.

%%-define(prepare_sql, true).
