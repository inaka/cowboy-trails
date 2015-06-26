-module(trails_handler).

%% API
-export([trails/1]).

%% @doc Returns the cowboy routes defined in the called module.
-callback trails(module()) -> cowboy_router:routes().

-spec trails(module()) -> cowboy_router:routes().
trails(Module) -> Module:trails().
