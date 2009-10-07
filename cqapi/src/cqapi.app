{application, cqapi,
 [{description, "cqapi"},
  {vsn, "0.1"},
  {modules, [
    cqapi,
    cqapi_app,
    cqapi_sup,
    cqapi_deps,
    cqapi_resource
  ]},
  {registered, []},
  {mod, {cqapi_app, []}},
  {env, [{db_host, {"localhost", 5984}}, {db, "carrotqueue"}]},
  {applications, [kernel, stdlib, crypto]}]}.
