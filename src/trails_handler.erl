%%% @doc Trails handler.
%%%      This behavior defines the callback `trails/0' which must be
%%%      implemented by the different `cowboy' handlers in your project.
-module(trails_handler).

%% API
-export([trails/1]).

%% @doc Returns the cowboy routes defined in the called module.
-callback trails() -> cowboy_router:routes().

-spec trails(module()) -> cowboy_router:routes().
trails(Module) -> Module:trails().
