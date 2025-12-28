-module(hot).
-export([call_handler/2]).

call_handler(Req, Master) ->
    neohook:http_handler(Req, Master).
