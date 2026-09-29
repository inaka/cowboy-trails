-module(trails_handler).
-moduledoc """
Trails handler.
Handlers can implement either the `trails/0` or `trails/1` callback to
expose the `cowboy` routes in the project.
""".

-export([trails/1]).

-doc """
Returns the cowboy routes defined in the called module.
""".
-callback trails() -> trails:trails().
-doc """
Returns the cowboy routes defined in the called module.
""".
-callback trails(Opts :: map()) -> trails:trails().

-optional_callbacks([trails/0, trails/1]).

-spec trails(module() | {module(), map()}) -> trails:trails().
trails({Module, Opts}) ->
    Module:trails(Opts);
trails(Module) ->
    Module:trails().
