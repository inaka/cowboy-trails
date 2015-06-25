-module(trails).

-export([compile/1]).

-spec compile(cowboy_router:routes()) -> cowboy_router:dispatch_rules().
compile(Routes) ->
  cowboy_router:compile(Routes).
