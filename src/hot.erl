-module(hot).
-export([make_handler/1, identity/1, db_receive_loop/2, db_send_loop/3]).

make_handler(State) ->
    fun(Req) -> neohook:http_handler_for_mist(Req, State) end.

identity(X) -> X.

db_receive_loop(Peer, Db) ->
    neohook:db_receive_loop(Peer, Db).

db_send_loop(Db, Peers, Me) ->
    neohook:db_send_loop(Db, Peers, Me).
