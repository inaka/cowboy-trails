-module(trails_handler).

%% @doc Returns the cowboy routes defined in the called module.
-callback trails() -> cowboy_router:routes().
