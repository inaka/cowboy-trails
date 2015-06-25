-module(trails).


-export([single_host_compile/1]).
-export([compile/1]).

-spec single_host_compile([cowboy_router:route_path()]) ->
  cowboy_router:dispatch_rules().
single_host_compile(Trails) ->
  compile([{'_', Trails}]).

-spec compile(cowboy_router:routes()) -> cowboy_router:dispatch_rules().
compile(Routes) ->
  cowboy_router:compile(Routes).
