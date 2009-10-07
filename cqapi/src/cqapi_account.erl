-module(cqapi_account).

-export([fetch_password/2, connect_to/1]).

%% TODO: Make this actually do something
fetch_password(User, _Realm) ->
  {ok, DBHost} = application:get_env(db_host),
  {ok, DB} = application:get_env(db),
  {Result} = couchbeam:query_view(DBHost, DB, "users", "all_users",
                                  [{<<"key">>, couchbeam_mochijson2:encode(list_to_binary(User))}],
                                 [{<<"group">>, <<"true">>),
  case extract_field(<<"password">>, Result) of
    not_found ->
      "";
    Password ->
      binary_to_list(Password)
  end.

connect_to(User) ->
  {ok, DBHost} = application:get_env(db_host),
  {ok, DB} = application:get_env(db),
  {Result} = couchbeam:query_view(DBHost, DB, "users", "all_users", [{<<"key">>, couchbeam_mochijson2:encode(list_to_binary(User))}]),
  case extract_field(<<"node">>, Result) of
    not_found ->
      throw({error, uknown_node, User});
    Node ->
      list_to_existing_atom(Node)
  end.

%% Internal functions
%% Internal functions
extract_field(FieldName, Result) ->
  Rows = proplists:get_value(<<"rows">>, Result),
  case Rows of
    [] ->
      not_found;
    _ ->
      [{Row}|_] = Rows,
      case proplists:get_value(<<"value">>, Row) of
        {} ->
          not_found;
        {Value} ->
          proplists:get_value(FieldName, Value, not_found)
      end
  end.
