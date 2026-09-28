-module(trails).
-moduledoc """
Trails main interface.
Use the functions provided in this module to inteact with `trails`.
""".

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
-export([store/1, store/2]).
-export([all/0, all/1, all/2]).
-export([retrieve/1, retrieve/2, retrieve/3]).
-export([api_root/0, api_root/1]).
-export([servers/0]).
-export([host_matches/1]).

-doc """
Trail specification.
""".
-opaque trail() ::
    #{
        path_match => route_match(),
        constraints => cowboy:fields(),
        handler => module(),
        options => term(),
        metadata => metadata(term())
    }.
-export_type([trail/0, route_match/0]).

-doc #{group => "Exported from `cowboy_router`"}.
-nominal route_match() :: '_' | iodata().
-doc #{group => "Exported from `cowboy_router`"}.
-nominal route_path() ::
    {Path :: route_match(), Handler :: module(), Opts :: term()}
    | {Path :: route_match(), cowboy:fields(), Handler :: module(), Opts :: term()}.
-doc #{group => "Exported from `cowboy_router`"}.
-nominal route_rule() ::
    {Host :: route_match(), Paths :: [route_path()]}
    | {Host :: route_match(), cowboy:fields(), Paths :: [route_path()]}.

-doc """
Trail specifications (With or without metadata).
""".
-type trails() :: [trail() | route_path()].
-export_type([trails/0]).

-doc """
HTTP Method.
""".
-nominal method() :: get | put | post | delete | patch | head | options.
-export_type([method/0]).

-doc """
Metadata associated with each HTTP method.
""".
-nominal metadata(X) :: #{method() => X}.
-export_type([metadata/1]).

-elvis([{elvis_style, no_throw, disable}]).

-doc #{equiv => compile([{'_', Trails}]), group => "API"}.
-spec single_host_compile(trails()) -> cowboy_router:dispatch_rules().
single_host_compile(Trails) ->
    compile([{'_', Trails}]).

-doc """
Compiles the given list of trails routes, also compatible with
`cowboy` routes.
""".
-doc #{group => "API"}.
-spec compile([{Host :: route_match(), Trails :: trails()}]) -> cowboy_router:dispatch_rules().
compile(Routes) ->
    cowboy_router:compile([{Host, to_route_paths(Trails)} || {Host, Trails} <:- Routes]).

-doc """
Translates the given trails paths into `cowboy` routes.
""".
-doc #{group => "API"}.
-spec to_route_paths([trail()]) -> cowboy_router:routes().
to_route_paths(Paths) ->
    lists:map(fun to_route_path/1, Paths).

-doc """
Translates a trail path into a route rule.
""".
-doc #{group => "API"}.
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

-doc #{equiv => trail(PathMatch, ModuleHandler, [], #{}, []), group => "API"}.
-spec trail(route_match(), module()) -> trail().
trail(PathMatch, ModuleHandler) ->
    trail(PathMatch, ModuleHandler, [], #{}, []).

-doc #{equiv => trail(PathMatch, ModuleHandler, Options, #{}, []), group => "API"}.
-spec trail(route_match(), module(), term()) -> trail().
trail(PathMatch, ModuleHandler, Options) ->
    trail(PathMatch, ModuleHandler, Options, #{}, []).

-doc #{equiv => trail(PathMatch, ModuleHandler, Options, MetaData, []), group => "API"}.
-spec trail(route_match(), module(), term(), map()) -> trail().
trail(PathMatch, ModuleHandler, Options, MetaData) ->
    trail(PathMatch, ModuleHandler, Options, MetaData, []).

-doc """
This function allows you to add additional information to the
`cowboy` handler, such as: resource path, handler module,
options and metadata. Normally used to document handlers.
""".
-doc #{group => "API"}.
-spec trail(route_match(), module(), term(), map(), cowboy:fields()) -> trail().
trail(PathMatch, ModuleHandler, Options, MetaData, Constraints) ->
    #{
        path_match => PathMatch,
        handler => ModuleHandler,
        options => Options,
        metadata => MetaData,
        constraints => Constraints
    }.

-doc """
Gets the `path_match` from the given `trail`.
""".
-doc #{group => "API"}.
-spec path_match(trail()) -> route_match().
path_match(Trail) ->
    maps:get(path_match, Trail, []).

-doc """
Gets the `handler` from the given `trail`.
""".
-doc #{group => "API"}.
-spec handler(trail()) -> module().
handler(Trail) ->
    maps:get(handler, Trail, []).

-doc """
Gets the `options` from the given `trail`.
""".
-doc #{group => "API"}.
-spec options(trail()) -> term().
options(Trail) ->
    maps:get(options, Trail, []).

-doc """
Gets the `metadata` from the given `trail`.
""".
-doc #{group => "API"}.
-spec metadata(trail()) -> map().
metadata(Trail) ->
    maps:get(metadata, Trail, #{}).

-doc """
Gets the `constraints` from the given `trail`.
""".
-doc #{group => "API"}.
-spec constraints(trail()) -> cowboy:fields().
constraints(Trail) ->
    maps:get(constraints, Trail, []).

-doc """
This function allows you to define the routes on each resource handler,
instead of defining them all in one place (as you're required to do
with `cowboy`). Your handler must implement the callback `c:trails_handler:trails/0`
and return the specific routes for that handler. That callback is
invoked for each given module and then the results are concatenated.
""".
-doc #{group => "API"}.
-spec trails(module() | [module()] | {module(), map()} | [{module(), map()}]) -> trails().
trails(Handlers) when is_list(Handlers) ->
    trails(Handlers, []);
trails(Handler) ->
    trails([Handler], []).

-doc """
Store the given list of trails.
""".
-doc #{equiv => store('_', Trails), group => "API"}.
-spec store(Trails :: trails() | [{HostMatch :: route_match(), Trails :: trails()}]) ->
    ok.
store(Trails) ->
    store('_', Trails).

-doc """
Store the given list of trails for a particular server.
""".
-doc #{group => "API"}.
-spec store(
    Server :: ranch:ref(),
    Trails :: trails() | [{HostMatch :: route_match(), Trails :: trails()}]
) ->
    ok.
store(_Server, []) ->
    ok;
store(Server, [{HostMatch, Trails} | Hosts]) ->
    NormalizedPaths = normalize_store_input(Trails),
    do_store(Server, HostMatch, NormalizedPaths),
    store(Server, Hosts);
store(Server, Trails) ->
    NormalizedPaths = normalize_store_input(Trails),
    do_store(Server, '_', NormalizedPaths).

-doc """
Retrieves all stored trails.
""".
-doc #{group => "API"}.
-spec all() -> [trail()].
all() ->
    all('_').

-doc """
Retrieves all stored trails for the given `HostMatch`
""".
-doc #{group => "API"}.
-spec all(HostMatch :: route_match()) -> [trail()].
all(HostMatch) ->
    all('_', HostMatch).

-doc """
Retrieves all stored trails for the given `Server` and `HostMatch`.
""".
-doc #{group => "API"}.
-spec all(Server :: ranch:ref(), HostMatch :: route_match()) -> [trail()].
all(Server, HostMatch) ->
    case application:get_application(trails) of
        {ok, trails} ->
            MatchSpec =
                case {Server, HostMatch} of
                    {'_', HostMatch} ->
                        [{{{'$1', HostMatch, '$2'}, '$3'}, [], ['$$']}];
                    {Server, HostMatch} ->
                        [{{{Server, HostMatch, '$1'}, '$2'}, [], ['$$']}]
                end,
            Matches = ets:select(trails, MatchSpec),
            FoundServers = [Srvr || [Srvr | _] = Match <:- Matches, length(Match) =:= 3],
            % Extract unique elements
            Servers = lists:usort(FoundServers),
            % There should be no more than one element in this list.
            % If there is more than one, it means the same trail is defined
            % in other host(s).
            case Servers of
                [_, _ | _] ->
                    throw(multiple_servers);
                _ ->
                    ok
            end,
            Trails = all_trails(Matches, []),
            SortIdFun = fun(A, B) -> maps:get(trails_id, A) < maps:get(trails_id, B) end,
            SortedStoredTrails = lists:sort(SortIdFun, Trails),
            lists:map(fun remove_id/1, SortedStoredTrails);
        _ ->
            throw({not_started, trails})
    end.

-doc """
Fetch the trail that matches with the given path.
""".
-doc #{group => "API"}.
-spec retrieve(PathMatch :: string()) -> trail() | notfound.
retrieve(PathMatch) ->
    retrieve('_', PathMatch).

-doc """
Fetch the trail that matches with the given host and path.
""".
-doc #{group => "API"}.
-spec retrieve(HostMatch :: route_match(), PathMatch :: string()) -> trail() | notfound.
retrieve(HostMatch, PathMatch) ->
    retrieve('_', HostMatch, PathMatch).

-doc """
Fetch the trail that matches with the given server and host and path.
""".
-doc #{group => "API"}.
-spec retrieve(
    Server :: ranch:ref(),
    HostMatch :: route_match(),
    PathMatch :: string()
) ->
    trail() | notfound.
retrieve(Server, HostMatch, PathMatch) ->
    case application:get_application(trails) of
        {ok, trails} ->
            Key = {Server, HostMatch, PathMatch},
            case ets:select(trails, [{{Key, '$1'}, [], ['$$']}]) of
                % No elements found
                [] ->
                    notfound;
                % One element found
                [[Trail = #{path_match := PathMatch}]] ->
                    remove_id(Trail);
                % More than one element found
                [_, _ | _] ->
                    throw(multiple_trails)
            end;
        _ ->
            throw({not_started, trails})
    end.

-doc """
Get api_root env param value if any, empty otherwise.
""".
-doc #{group => "API"}.
-spec api_root() -> string().
api_root() ->
    application:get_env(trails, api_root, "").

-doc """
Set api_root env param to the given `Path`.
""".
-doc #{group => "API"}.
-spec api_root(string()) -> ok.
api_root(Path) ->
    application:set_env(trails, api_root, Path).

-doc """
Returns the list of known servers.
""".
-doc #{group => "API"}.
-spec servers() -> [ranch:ref()].
servers() ->
    lists:flatten(
        ets:match(ranch_server, {{conns_sup, '$1', '_'}, '_'})
    ).

-doc """
Returns the list of host matches for a given server.
""".
-doc #{group => "API"}.
-spec host_matches(ranch:ref()) -> [route_match()].
host_matches(ServerRef) ->
    [Opts] = lists:flatten(ets:match(ranch_server, {{proto_opts, ServerRef}, '$1'})),
    Env = maps:get(env, Opts, #{}),
    Dispatches = maps:get(dispatch, Env, []),
    lists:flatten([Host || {Host, _, _} <:- Dispatches]).

-doc false.
trails([], Acc) ->
    Acc;
trails([Module | T], Acc) ->
    trails(T, Acc ++ trails_handler:trails(Module)).

-doc false.
-spec do_store(
    Server :: ranch:ref(),
    HostMatch :: route_match(),
    Trails :: [route_path()]
) ->
    ok.
do_store(_Server, _HostMatch, []) ->
    ok;
do_store(Server, HostMatch, [Trail = #{path_match := PathMatch} | Trails]) ->
    ets:insert(trails, {{Server, HostMatch, PathMatch}, Trail}),
    do_store(Server, HostMatch, Trails).

-doc false.
all_trails([], Acc) ->
    Acc;
all_trails([[_, Trail] | T], Acc) ->
    all_trails(T, [Trail | Acc]);
all_trails([[_Server, _PathMatch, Trail] | T], Acc) ->
    all_trails(T, [Trail | Acc]).

-doc false.
-spec normalize_store_input(trails()) -> [map()].
normalize_store_input(RoutesPaths) ->
    normalize_id(normalize_paths(RoutesPaths)).

-spec normalize_id([route_path()]) -> [map()].
normalize_id(Trails) ->
    Length = length(Trails),
    AddIdFun = fun(Trail, Id) -> Trail#{trails_id => Id} end,
    lists:zipwith(AddIdFun, Trails, lists:seq(1, Length)).

-doc false.
-spec normalize_paths(trails()) -> [trail()].
normalize_paths(RoutesPaths) ->
    lists:map(fun normalize_path/1, RoutesPaths).

-doc false.
-spec remove_id(trail()) -> trail().
remove_id(Trail) ->
    maps:remove(trails_id, Trail).

-doc false.
-spec normalize_path(route_path() | trail()) -> trail().
normalize_path({PathMatch, ModuleHandler, Options}) ->
    trail(PathMatch, ModuleHandler, Options);
normalize_path({PathMatch, Constraints, ModuleHandler, Options}) ->
    trail(PathMatch, ModuleHandler, Options, #{}, Constraints);
normalize_path(Trail) when is_map(Trail) ->
    Trail.
