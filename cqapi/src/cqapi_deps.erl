%% Copyright (c) 2009 Hypothetical Labs, Inc.

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

%% @doc Ensure that the relatively-installed dependencies are on the code
%%      loading path, and locate resources relative
%%      to this application's path.

-module(cqapi_deps).
-author('author <author@example.com>').

-export([ensure/0, ensure/1]).
-export([get_base_dir/0, get_base_dir/1]).
-export([local_path/1, local_path/2]).
-export([deps_on_path/0, new_siblings/1]).

%% @spec deps_on_path() -> [ProjNameAndVers]
%% @doc List of project dependencies on the path.
deps_on_path() ->
    F = fun (X, Acc) ->
                ProjDir = filename:dirname(X),
                case {filename:basename(X),
                      filename:basename(filename:dirname(ProjDir))} of
                    {"ebin", "deps"} ->
                        [filename:basename(ProjDir) | Acc];
                    _ ->
                        Acc
                end
        end,
    ordsets:from_list(lists:foldl(F, [], code:get_path())).

%% @spec new_siblings(Module) -> [Dir]
%% @doc Find new siblings paths relative to Module that aren't already on the
%%      code path.
new_siblings(Module) ->
    Existing = deps_on_path(),
    SiblingEbin = filelib:wildcard(local_path(["deps", "*", "ebin"], Module)),
    Siblings = [filename:dirname(X) || X <- SiblingEbin,
                           ordsets:is_element(
                             filename:basename(filename:dirname(X)),
                             Existing) =:= false],
    lists:filter(fun filelib:is_dir/1,
                 lists:append([[filename:join([X, "ebin"]),
                                filename:join([X, "include"])] ||
                                  X <- Siblings])).


%% @spec ensure(Module) -> ok
%% @doc Ensure that all ebin and include paths for dependencies
%%      of the application for Module are on the code path.
ensure(Module) ->
    code:add_paths(new_siblings(Module)),
    code:clash(),
    ok.

%% @spec ensure() -> ok
%% @doc Ensure that the ebin and include paths for dependencies of
%%      this application are on the code path. Equivalent to
%%      ensure(?Module).
ensure() ->
    ensure(?MODULE).

%% @spec get_base_dir(Module) -> string()
%% @doc Return the application directory for Module. It assumes Module is in
%%      a standard OTP layout application in the ebin or src directory.
get_base_dir(Module) ->
    {file, Here} = code:is_loaded(Module),
    filename:dirname(filename:dirname(Here)).

%% @spec get_base_dir() -> string()
%% @doc Return the application directory for this application. Equivalent to
%%      get_base_dir(?MODULE).
get_base_dir() ->
    get_base_dir(?MODULE).

%% @spec local_path([string()], Module) -> string()
%% @doc Return an application-relative directory from Module's application.
local_path(Components, Module) ->
    filename:join([get_base_dir(Module) | Components]).

%% @spec local_path(Components) -> string()
%% @doc Return an application-relative directory for this application.
%%      Equivalent to local_path(Components, ?MODULE).
local_path(Components) ->
    local_path(Components, ?MODULE).
