-module(cqapi_digest_auth).

-include("cqapi.hrl").

-export([handle_auth/4, is_authed/2, auth_head/2]).

handle_auth(AuthRealm, AuthFun, ReqData, State) ->
  case cqapi_digest_auth:is_authed(ReqData, AuthFun) of
    false ->
      {cqapi_digest_auth:auth_head(AuthRealm, ReqData), ReqData, State};
    {true, Acct} ->
      {ok, Node} = cqapi_account:connect_to(Acct),
      {true, ReqData, State#resource_state{acct=Acct, node=Node}}
  end.


%% @edoc Inspects a webmachine request to determine if the client has auth'd
%% @spec is_authed(any() -> true | false
is_authed(ReqData, PassFun) ->
  case parse_auth_header(ReqData) of
    [] ->
      false;
    AuthData ->
      Response = proplists:get_value("response", AuthData),
      ServerResponse = hash_sequence([calc_ha1(AuthData, PassFun)|nonce_list(AuthData)]++[calc_ha2(ReqData, AuthData)]),
      if
        Response =:= ServerResponse ->
          {true, proplists:get_value("username", AuthData)};
        true ->
          false
      end
  end.

auth_head(Realm, ReqData) ->
  Nonce = md5(lists:flatten(io_lib:format("~s|~p", [wrq:peer(ReqData), erlang:now()]))),
  cqapi_digest_header:generate([{"realm",Realm},{"qop","auth"},{"nonce",Nonce},{"algorithm","MD5-sess"}]).

calc_ha1(AuthData, PassFun) ->
  User = proplists:get_value("username", AuthData),
  Realm = proplists:get_value("realm", AuthData),
  Password = PassFun(User, Realm),
  HA1 = hash_sequence([User,Realm,Password]),
  case string:to_lower(proplists:get_value("algorithm", AuthData)) of
    "md5-sess" ->
      hash_sequence([HA1, proplists:get_value("nonce", AuthData), proplists:get_value("cnonce", AuthData)]);
    _ -> HA1
  end.

calc_ha2(ReqData, AuthData) ->
  HA2 = case proplists:get_value("qop", AuthData) of
          "auth-int" ->
            case wrq:req_body(ReqData) of
              undefined -> [];
              Binary -> [md5(Binary)]
            end;
           _ -> []
        end,
  hash_sequence([atom_to_list(wrq:method(ReqData)),proplists:get_value("uri", AuthData)|HA2]).

nonce_list(AuthData) ->
  Nonce = proplists:get_value("nonce", AuthData),
  NC = proplists:get_value("nc", AuthData),
  CNonce = proplists:get_value("cnonce", AuthData),
  QOP = proplists:get_value("qop", AuthData),
  [Nonce, NC, CNonce, QOP].

parse_auth_header(ReqData) ->
  case wrq:get_req_header("Authorization", ReqData) of
    undefined ->
      [];
    AuthHeader ->
      case cqapi_digest_header:parse(AuthHeader) of
        Term when not is_list(Term) -> [];
        PropList -> PropList
      end
  end.

hash_sequence(List) when is_list(List) ->
  md5(string:join(List, ":")).

md5(Value) ->
  mochihex:to_hex(erlang:md5(Value)).
