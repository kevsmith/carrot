-record(resource_state, {acct,
                         node,
                         data}).

-define(AUTH_REALM, "api.carrotqueue.com").

-define(AUTH_FUN, fun cqapi_account:fetch_password/2).
