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
-export([store/1, all/0, retrieve/1]).


-opaque trail() ::
  #{ path_match  => cowboy_router:route_match()
   , constraints => cowboy_router:constraints()
   , handler     => module()
   , options     => any()
   , metadata    => metadata()
   }.
-export_type([trail/0]).

%% Exported from cowboy_router.erl
-type route_match() :: '_' | iodata().
-type route_path() :: {Path::route_match(), Handler::module(), Opts::any()}
  | {Path::route_match()
    , cowboy_router:constraints()
    , Handler::module()
    , Opts::any()}.
-type route_rule() :: {Host::route_match(), Paths::[route_path()]}
  | {Host::route_match(), cowboy_router:constraints(), Paths::[route_path()]}.
%% End of exported functions

-type trails() :: [trails:trail() | route_path()].
-export_type([trails/0]).

-type method() :: get | put | post | delete | patch | head | options.
-export_type([method/0]).

-type metadata() :: #{method() => map()}.
-export_type([metadata/0]).

-spec single_host_compile([route_path()]) ->
  cowboy_router:dispatch_rules().
single_host_compile(Trails) ->
  compile([{'_', Trails}]).

-spec compile([{Host::route_match(), Trails::trails()}]) ->
  cowboy_router:dispatch_rules().
compile([]) -> [];
compile(Routes) ->
  cowboy_router:compile(
    [{Host, to_route_paths(Trails)} || {Host, Trails} <- Routes]).

-spec to_route_paths([trail()]) -> cowboy_router:routes().
to_route_paths(Paths) ->
  [to_route_path(Path)|| Path <- Paths].

-spec to_route_path(trail()) -> route_rule().
to_route_path(Trail) when is_map(Trail) ->
  PathMatch = maps:get(path_match, Trail),
  ModuleHandler = maps:get(handler, Trail),
  Options = maps:get(options, Trail, []),
  Constraints = maps:get(constraints, Trail, []),
  {PathMatch, Constraints, ModuleHandler, Options}
  ;
to_route_path(Trail) when is_tuple(Trail) ->
  Trail.

-spec trail(route_match(), module()) -> trail().
trail(PathMatch, ModuleHandler) ->
  trail(PathMatch, ModuleHandler, [], #{}, []).

-spec trail(route_match(), module(), any()) -> trail().
trail(PathMatch, ModuleHandler, Options) ->
  trail(PathMatch, ModuleHandler, Options, #{}, []).

-spec trail(route_match(), module(), any(), map()) -> trail().
trail(PathMatch, ModuleHandler, Options, MetaData) ->
  trail(PathMatch, ModuleHandler, Options, MetaData, []).

-spec trail(route_match()
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

-spec store([trail()]) -> ok.
store(Trails) ->
  application:ensure_all_started(trails),
  NormalizedPaths = normalize_store_input(Trails),
  store1(NormalizedPaths).

-spec all() -> [trail()].
all() ->
  case application:get_application(trails) of
    {ok, trails} ->
      Trails = all(ets:select(trails, [{{'$1', '$2'}, [], ['$$']}]), []),
      SortIdFun =
        fun(A, B) -> maps:get(trails_id, A) < maps:get(trails_id, B) end,
      SortedStoredTrails = lists:sort(SortIdFun, Trails),
     lists:map(fun remove_id/1, SortedStoredTrails);
    _ ->
      throw({not_started, trails})
  end.

-spec retrieve(route_match()) -> trail().
retrieve(Path) ->
  case application:get_application(trails) of
    {ok, trails} ->
      case ets:lookup(trails, Path) of
        [{_, Val}] -> remove_id(Val);
        []         -> notfound
      end;
    _ ->
      throw({not_started, trails})
  end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
trails([], Acc) ->
  Acc;
trails([Module | T], Acc) ->
  trails(T, Acc ++ trails_handler:trails(Module)).

%% @private
store1([]) ->
  ok;
store1([Trail = #{path_match := Key} | T]) ->
  ets:insert(trails, {Key, Trail}),
  store1(T).

%% @private
all([], Acc) ->
  Acc;
all([[_, Trail] | T], Acc) ->
  all(T, [Trail | Acc]).

%% @private
-spec normalize_store_input([route_path()]) -> trails().
normalize_store_input(RoutesPaths) ->
  normalize_id(normalize_paths(RoutesPaths)).

-spec normalize_id([route_path()]) -> trails().
normalize_id(Trails) ->
  Length = length(Trails),
  AddIdFun = fun(Trail, Id) -> Trail#{ trails_id => Id} end,
  lists:zipwith(AddIdFun, Trails, lists:seq(1, Length)).

%% @private
-spec normalize_paths([route_path()]) -> trails().
normalize_paths(RoutesPaths) ->
  [normalize_path(Path) || Path <- RoutesPaths].

%% @private
-spec remove_id(trail()) -> trail().
remove_id(Trail) -> maps:remove(trails_id, Trail).

%% @private
-spec normalize_path(route_path() | trail()) -> trail().
normalize_path({PathMatch, ModuleHandler, Options}) ->
  trail(PathMatch, ModuleHandler, Options);
normalize_path({PathMatch, Constraints, ModuleHandler, Options}) ->
  trail(PathMatch, ModuleHandler, Options, #{}, Constraints);
normalize_path(Trail) when is_map(Trail) -> Trail.
