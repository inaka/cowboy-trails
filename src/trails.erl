-module(trails).

-export([single_host_compile/1]).
-export([compile/1]).
-export([trail/2]).
-export([trail/3]).
-export([trail/4]).
-export([trails/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec single_host_compile([cowboy_router:route_path()]) ->
  cowboy_router:dispatch_rules().
single_host_compile(Trails) ->
  compile([{'_', Trails}]).

-spec compile(cowboy_router:routes()) -> cowboy_router:dispatch_rules().
compile(Routes) ->
  cowboy_router:compile(Routes).

-spec trail(cowboy_router:route_match()
          , module()) -> cowboy_router:route_path().
trail(PathMatch, ModuleHandler) ->
  trail(PathMatch, ModuleHandler, []).

-spec trail(cowboy_router:route_match()
          , module()
          , any()) -> cowboy_router:route_path().
trail(PathMatch, ModuleHandler, Options) ->
  {PathMatch, ModuleHandler, Options}.

-spec trail(cowboy_router:route_match()
          , cowboy_router:constraints()
          , module()
          , any()) -> cowboy_router:route_path().
trail(PathMatch, Constraints, ModuleHandler, Options) ->
  {PathMatch, Constraints, ModuleHandler, Options}.

-spec trails(module() | [module()]) -> cowboy_router:routes().
trails(Handlers) when is_list(Handlers) ->
  trails(Handlers, []);
trails(Handler) ->
  trails([Handler], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
trails([], Acc) ->
  Acc;
trails([Module | T], Acc) ->
  trails(T, Module:trails() ++ Acc).
