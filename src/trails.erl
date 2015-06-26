-module(trails).

-export([single_host_compile/1]).
-export([compile/1]).
-export([trail/2]).
-export([trail/3]).
-export([trail/4]).
-export([trail/5]).
-export([trails/1]).
-export([path_match/1]).
-export([handler/1]).
-export([options/1]).
-export([metadata/1]).
-export([constraints/1]).

-opaque trail() ::
  #{ path_match  => integer()
   , constraints => integer()
   , handler     => module()
   , options     => any()
   , metadata    => map()
   }.
-export_type([trail/0]).0

-type trails() :: [ trails:trail() | cowboy_router:route_path() ].
-export_type([trails/0]).

-type trails() :: [ trails:trail() | cowboy_router:route_path() ].
-export_type([trails/0]).

-spec single_host_compile([cowboy_router:route_path()]) ->
  cowboy_router:dispatch_rules().
single_host_compile(Trails) ->
  compile([{'_', Trails}]).

-spec compile([{Host::cowboy_router:route_match(), Trails::trails()}]) ->
  cowboy_router:dispatch_rules().
compile([]) -> [];
compile(Routes) ->
  cowboy_router:compile(
    [{Host, to_route_paths(Trails)} || {Host, Trails} <- Routes]).

  -spec to_route_paths(trail()) -> cowboy_router:route_path().
to_route_paths(Paths) ->
  [to_route_path(Path)|| Path <- Paths].

-spec to_route_path(trail()) -> cowboy_router:route_path().
to_route_path(Trail) when is_map(Trail) ->
  PathMatch = maps:get(path_match, Trail),
  ModuleHandler = maps:get(handler, Trail),
  Options = maps:get(options, Trail, []),
  Constraints = maps:get(constraints, Trail, []),
  {PathMatch, Constraints, ModuleHandler, Options}
  ;
to_route_path(Trail) when is_tuple(Trail) ->
  Trail.

-spec trail(cowboy_router:route_match()
          , module()) -> cowboy_router:route_path().
trail(PathMatch, ModuleHandler) ->
  trail(PathMatch, ModuleHandler, [], #{}, []).

-spec trail(cowboy_router:route_match()
          , module()
          , any()) -> cowboy_router:route_path().
trail(PathMatch, ModuleHandler, Options) ->
  trail(PathMatch, ModuleHandler, Options, #{}, []).

-spec trail(cowboy_router:route_match()
          , module()
          , any()
          , map()) -> cowboy_router:route_path().
trail(PathMatch, ModuleHandler, Options, MetaData) ->
  trail(PathMatch, ModuleHandler, Options, MetaData, []).


-spec trail(cowboy_router:route_match()
          , module()
          , any()
          , map()
         , cowboy_router:constraints()) -> trail().
trail(PathMatch, ModuleHandler, Options, MetaData, Constraints) ->
    #{ path_match  => PathMatch
     , handler     => ModuleHandler
     , options     => Options
     , metadata    => MetaData
     , constraints => Constraints
     }.

-spec path_match(map()) -> string() | binary().
path_match(Trail) ->
 maps:get(path_match, Trail, []).

-spec handler(map()) -> module().
handler(Trail) ->
 maps:get(handler, Trail, []).

 -spec options(map()) -> list().
options(Trail) ->
 maps:get(options, Trail, []).

 -spec metadata(map()) -> map().
metadata(Trail) ->
 maps:get(metadata, Trail, []).

 -spec constraints(map()) -> list().
constraints(Trail) ->
 maps:get(constraints, Trail, []).

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
  trails(T, Acc ++ trails_handler:trails(Module)).

