-module(hot).
-export([make_handler/1, identity/1, db_receive_loop/1, db_send_loop/1]).

make_handler(State) ->
    fun(Req) -> neohook:http_handler(Req, State) end.

identity(X) -> X.

db_receive_loop(Db) ->
    neohook:db_receive_loop(Db).

db_send_loop(Db) ->
    neohook:db_send_loop(Db).
