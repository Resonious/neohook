#!/bin/sh
set -eu

PACKAGE=neohook
BASE=$(dirname "$0")
COMMAND="${1:-run}"

run() {
  exec erl \
    -pa "$BASE"/*/ebin \
    -eval "$PACKAGE@@main:run($PACKAGE)" \
    -noshell \
    -sname neohook \
    -setcookie neohook_cookie \
    -extra "$@"
}

shell() {
  erl -pa "$BASE"/*/ebin
}

reload() {
  NODE="neohook@$(hostname -s)"
  erl -sname "reload_$$" -setcookie neohook_cookie -noshell \
    -eval "
      case net_kernel:connect_node('$NODE') of
        true ->
          Modules = [hot, neohook, pipe, pipemaster, env, tls, json_pretty, termcolor],
          Results = [begin
            PurgeResult = rpc:call('$NODE', code, purge, [M]),
            LoadResult = rpc:call('$NODE', code, load_file, [M]),
            io:format(\"~p: purge=~p load=~p~n\", [M, PurgeResult, LoadResult]),
            LoadResult
          end || M <- Modules],
          io:format(\"~nReloaded ~p modules~n\", [length(Modules)]);
        false ->
          io:format(\"Failed to connect to ~s~n\", ['$NODE']),
          halt(1)
      end,
      init:stop().
    "
}

case "$COMMAND" in
run)
  shift || true
  run "$@"
  ;;

shell)
  shell
  ;;

reload)
  reload
  ;;

*)
  echo "usage:" >&2
  echo "  entrypoint.sh \$COMMAND" >&2
  echo "" >&2
  echo "commands:" >&2
  echo "  run     Run the project main function" >&2
  echo "  shell   Run an Erlang shell" >&2
  echo "  reload  Hot reload modules in running node" >&2
  exit 1
  ;;
esac
