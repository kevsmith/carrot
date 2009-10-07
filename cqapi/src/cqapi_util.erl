-module(cqapi_util).

-export([fixup_vhost/1, get_request_uri/1]).

fixup_vhost(VHost) ->
  case VHost of
    ["/"|_] ->
      VHost;
    _ ->
      "/" ++ VHost
  end.

get_request_uri(ReqData) ->
  HostWithPort = wrq:get_req_header("Host", ReqData),
  RawPath = wrq:raw_path(ReqData),
  lists:flatten(["http://", HostWithPort, RawPath]).
