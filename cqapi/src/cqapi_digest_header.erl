-module(cqapi_digest_header).
-export([parse/1,file/1,generate/1]).
-include_lib("neotoma/include/peg.hrl").

rule(header) ->
  peg:seq([peg:string("Digest "), fun pairs/2]);

rule(pairs) ->
  peg:seq([peg:label('head', fun pair/2), peg:label('tail', peg:zero_or_more(peg:seq([peg:string(","), peg:zero_or_more(fun ws/2), fun pair/2])))]);

rule(pair) ->
  peg:seq([fun key/2, peg:string("="), peg:choose([fun quoted_value/2, fun value/2])]);

rule(key) ->
  peg:one_or_more(peg:charclass("[A-Za-z]"));

rule(value) ->
  peg:one_or_more(peg:seq([peg:not_(fun ws/2), peg:not_(peg:string(",")), peg:anything()]));

rule(quoted_value) ->
  peg:seq([peg:string("\""), peg:label('value', peg:zero_or_more(peg:seq([peg:not_(peg:string("\"")), peg:anything()]))), peg:string("\"")]);

rule(ws) ->
  peg:charclass("[ \t\n\r]").

transform(header, [_,Pairs],_) -> Pairs;
transform(pairs, List,_) ->
  Head = proplists:get_value(head, List),
  Tail = [Pair || [_,_,Pair] <- proplists:get_value(tail, List)],
  [Head|Tail];
transform(pair, [Key,_,Value], _) ->
  {Key, Value};
transform(key, Node, _) ->
  lists:flatten(Node);
transform(quoted_value, [_,{'value', Value},_], _) ->
  lists:flatten(Value);
transform(value, Node, _) ->
  lists:flatten(Node);
transform(_,Node,_Index) -> Node.

generate(List) when is_list(List) ->
  lists:flatten([
                 "Digest ",
                 string:join([pair_to_string(P) || P <- List], ", ")
                ]).

pair_to_string({"nc",Value}) ->
  lists:flatten(["nc=",Value]);
pair_to_string({Key, Value}) ->
  lists:flatten([Key, $=, $", Value, $"]).
