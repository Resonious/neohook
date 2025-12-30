-module(hot).
-export([make_handler/1, identity/1]).

make_handler(Master) ->
    fun(Req) -> neohook:http_handler(Req, Master) end.

identity(X) -> X.
