-module(hot).
-export([make_handler/1, identity/1, db_sync_loop/1]).

make_handler(State) ->
    fun(Req) -> neohook:http_handler(Req, State) end.

identity(X) -> X.

db_sync_loop(Db) ->
    neohook:db_sync_loop(Db).
