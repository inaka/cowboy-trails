%%% @doc Trails handler.
%%%      This behavior defines the callback `c:trails/0' which must be
%%%      implemented by the different `cowboy' handlers in your project.
-module(trails_handler).

%% API
-export([trails/1]).

%% @doc Returns the cowboy routes defined in the called module.
-callback trails() -> trails:trails().
-callback trails(Opts :: map()) -> trails:trails().

-optional_callbacks([trails/0, trails/1]).

-spec trails(module() | {module(), map()}) -> trails:trails().
trails({Module, Opts}) ->
    Module:trails(Opts);
trails(Module) ->
    Module:trails().
