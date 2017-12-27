#!/usr/bin/env escript
%% -*- erlang -*-

main([NodeCtrl, Node, Cookie, _]) ->
     {ok, _} = net_kernel:start([list_to_atom(NodeCtrl), longnames]),
     erlang:set_cookie(node(), list_to_atom(Cookie)),
     Res = rpc:call(list_to_atom(Node), init, stop, []),
     io:fwrite("==> ~p~n", [Res]).
