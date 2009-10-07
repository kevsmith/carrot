-module(wrq2).

-export([path_info/3]).

path_info(Key, ReqData, Missing) when is_function(Missing) ->
  case wrq:path_info(Key, ReqData) of
    undefined ->
      Missing();
    V ->
      V
  end;
path_info(Key, ReqData, Missing) ->
  case wrq:path_info(Key, ReqData) of
    undefined ->
      Missing;
    V ->
      V
  end.
