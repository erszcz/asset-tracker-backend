-module(tr_json).
-export([decode/1,
         encode/1,
         pretty_encode/1]).

decode(B) when is_binary(B) ->
    jsone:decode(B, [{object_format, map}]).

encode(Term) -> jsone:encode(Term).

pretty_encode(Term) ->
    jsone:encode(Term, [{indent, 1}, {space, 2}]).
