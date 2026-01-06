-module(fs).
-export([delete_files_matching/1]).

delete_files_matching(Pattern) ->
    Files = filelib:wildcard(binary_to_list(Pattern)),
    lists:foreach(fun(File) -> file:delete(File) end, Files),
    nil.
