%%% @doc Trails main interface.
%%%      Use the functions provided in this module to inteact with `trails'.
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
-export([api_root/0, api_root/1]).

%% Trail specification
-opaque trail() ::
  #{ path_match  => cowboy_router:route_match()
   , constraints => cowboy_router:constraints()
   , handler     => module()
   , options     => any()
   , metadata    => metadata(any())
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

-type metadata(X) :: #{method() => X}.
-export_type([metadata/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @equiv compile([{'_', Trails}])
-spec single_host_compile([route_path()]) ->
  cowboy_router:dispatch_rules().
single_host_compile(Trails) ->
  compile([{'_', Trails}]).

%% @doc Compiles the given list of trails routes, also compatible with
%%      `cowboy' routes.
-spec compile([{Host::route_match(), Trails::trails()}]) ->
  cowboy_router:dispatch_rules().
compile([]) -> [];
compile(Routes) ->
  cowboy_router:compile(
    [{Host, to_route_paths(Trails)} || {Host, Trails} <- Routes]).

%% @doc Translates the given trails paths into `cowboy' routes.
-spec to_route_paths([trail()]) -> cowboy_router:routes().
to_route_paths(Paths) ->
  [to_route_path(Path) || Path <- Paths].

%% @doc Translates a trail path into a route rule.
-spec to_route_path(trail()) -> route_rule().
to_route_path(Trail) when is_map(Trail) ->
  ApiRoot = api_root(),
  PathMatch = maps:get(path_match, Trail),
  ModuleHandler = maps:get(handler, Trail),
  Options = maps:get(options, Trail, []),
  Constraints = maps:get(constraints, Trail, []),
  {ApiRoot ++ PathMatch, Constraints, ModuleHandler, Options};
to_route_path(Trail) when is_tuple(Trail) ->
  Trail.

%% @equiv trail(PathMatch, ModuleHandler, [], #{}, [])
-spec trail(route_match(), module()) -> trail().
trail(PathMatch, ModuleHandler) ->
  trail(PathMatch, ModuleHandler, [], #{}, []).

%% @equiv trail(PathMatch, ModuleHandler, Options, #{}, [])
-spec trail(route_match(), module(), any()) -> trail().
trail(PathMatch, ModuleHandler, Options) ->
  trail(PathMatch, ModuleHandler, Options, #{}, []).

%% @equiv trail(PathMatch, ModuleHandler, Options, MetaData, [])
-spec trail(route_match(), module(), any(), map()) -> trail().
trail(PathMatch, ModuleHandler, Options, MetaData) ->
  trail(PathMatch, ModuleHandler, Options, MetaData, []).

%% @doc This function allows you to add additional information to the
%%      `cowboy' handler, such as: resource path, handler module,
%%      options and metadata. Normally used to document handlers.
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

%% @doc Gets the `path_match' from the given `trail'.
-spec path_match(map()) -> string() | binary().
path_match(Trail) ->
 maps:get(path_match, Trail, []).

%% @doc Gets the `handler' from the given `trail'.
-spec handler(map()) -> module().
handler(Trail) ->
 maps:get(handler, Trail, []).

%% @doc Gets the `options' from the given `trail'.
 -spec options(map()) -> list().
options(Trail) ->
 maps:get(options, Trail, []).

%% @doc Gets the `metadata' from the given `trail'.
 -spec metadata(map()) -> map().
metadata(Trail) ->
 maps:get(metadata, Trail, []).

%% @doc Gets the `constraints' from the given `trail'.
 -spec constraints(map()) -> list().
constraints(Trail) ->
 maps:get(constraints, Trail, []).

%% @doc This function allows you to define the routes on each resource handler,
%%      instead of defining them all in one place (as you're required to do
%%      with `cowboy'). Your handler must implement the callback `trails/0'
%%      and return the specific routes for that handler. That callback is
%%      invoked for each given module and then the results are concatenated.
-spec trails(module() | [module()]) -> trails:trails().
trails(Handlers) when is_list(Handlers) ->
  trails(Handlers, []);
trails(Handler) ->
  trails([Handler], []).

%% @doc Store the given list of trails.
-spec store(trails()) -> ok.
store(Trails) ->
  application:ensure_all_started(trails),
  NormalizedPaths = normalize_store_input(Trails),
  store1(NormalizedPaths).

%% @doc Retrieves all stored trails.
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

%% @doc Fetch the trail that matches with the given path.
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

%% @doc Get api_root env param value if any, empty otherwise.
-spec api_root() -> list().
api_root() ->
    application:get_env(trails, api_root, "").

%% @doc Set api_root env param to the given Path.
-spec api_root(list()) -> ok.
api_root(Path) ->
    application:set_env(trails, api_root, Path).

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
