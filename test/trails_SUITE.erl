-module(trails_SUITE).
-author('elbrujohalcon@inaka.net').

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([basic_compile_test/1]).
-export([minimal_compile_test/1]).
-export([static_compile_test/1]).
-export([minimal_single_host_compile_test/1]).
-export([basic_single_host_compile_test/1]).
-export([static_single_host_compile_test/1]).
-export([basic_trails2_constructor/1]).
-export([basic_trails3_constructor/1]).
-export([basic_trails4_constructor/1]).
-export([static_trails3_constructor/1]).
-export([static_trails4_constructor/1]).
-export([basic_metadata/1]).
-export([basic_trails_routes/1]).
-export([put_metadata/1]).
-export([post_metadata/1]).
-export([trails_store/1]).
-export([trails_api_root/1]).
-export([minimal_multiple_host_compile_test/1]).
-export([minimal_multiple_server_test/1]).
-export([server_hostmatches/1]).

-dialyzer([{no_opaque, [trails_api_root/1]}]).
-dialyzer([{no_return, [trails_api_root/1]}]).


-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, F /= module_info].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  _ = application:ensure_all_started(cowboy),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(trails_api_root, Config) ->
  meck:new(cowboy_router, [passthrough]),
  meck:expect(cowboy_router, compile, fun([_Routes]) -> ok end),
  Config;
init_per_testcase(_, Config) -> Config.

-spec end_per_testcase(atom(), config()) ->
  term() | {fail, term()} | {save_config, config()}.
end_per_testcase(trails_api_root, Config) ->
  meck:unload(cowboy_router),
  application:set_env(trails, api_root, ""),
  Config;
end_per_testcase(_, Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec minimal_compile_test(config()) -> {atom(), string()}.
minimal_compile_test(_Config) ->
  MininalRoute = [{'_', []}],
  ExpectedResponse = cowboy_router:compile(MininalRoute),
  ExpectedResponse = trails:compile(MininalRoute),
  {comment, ""}.

-spec basic_compile_test(config()) -> {atom(), string()}.
basic_compile_test(_Config) ->
  BasicRoute =
    [
      {"localhost",
        [
          {"/such/path", http_such_path_handler, []},
          {"/very", http_very, []}
       ]}
    ],
  ExpectedResponse = cowboy_router:compile(BasicRoute),
  ExpectedResponse = trails:compile(BasicRoute),
  {comment, ""}.

-spec static_compile_test(config()) -> {atom(), string()}.
static_compile_test(_Config) ->
  StaticRoute = get_static_route() ,
  ExpectedResponse = cowboy_router:compile(StaticRoute),
  ExpectedResponse = trails:compile(StaticRoute),
  {comment, ""}.

-spec minimal_single_host_compile_test(config()) -> {atom(), string()}.
minimal_single_host_compile_test(_Config) ->
  MininalRoute = [{'_', []}],
  [{_SingleHost, MininalPath}] = MininalRoute,
  ExpectedResponse = cowboy_router:compile(MininalRoute),
  ExpectedResponse = trails:single_host_compile(MininalPath),
  {comment, ""}.

-spec basic_single_host_compile_test(config()) -> {atom(), string()}.
basic_single_host_compile_test(_Config) ->
  BasicRoute = get_basic_route(),
  ExpectedResponse = cowboy_router:compile(BasicRoute),
  [{_SingleHost, BasicPath}] = BasicRoute,
  ExpectedResponse = trails:single_host_compile(BasicPath),
  {comment, ""}.

-spec static_single_host_compile_test(config()) -> {atom(), string()}.
static_single_host_compile_test(_Config) ->
  StaticRoute = get_static_route(),
  [{_SingleHost, StaticPath}] = StaticRoute,
  ExpectedResponse = cowboy_router:compile(StaticRoute),
  ExpectedResponse = trails:single_host_compile(StaticPath),
  {comment, ""}.

basic_trails2_constructor(_Config) ->
  BasicRoute =
    [
      {'_',
        [
          trails:trail("/such/path", http_basic_route),
          trails:trail("/very", http_very),
          trails:trail("/", http_handler)
       ]}
    ],
  BasicRouteCowboy = get_basic_route(),
  ExpectedResponse = cowboy_router:compile(BasicRouteCowboy),
  ExpectedResponse = trails:compile(BasicRoute),
  {comment, ""}.

basic_trails3_constructor(_Config) ->
  BasicRoute =
    [
      {'_',
        [
          trails:trail("/such/path", http_basic_route, []),
          trails:trail("/very", http_very, []),
          trails:trail("/", http_handler, [])
       ]}
    ],
  BasicRouteCowboy = get_basic_route(),
  ExpectedResponse = cowboy_router:compile(BasicRouteCowboy),
  ExpectedResponse = trails:compile(BasicRoute),
  {comment, ""}.

-spec static_trails3_constructor(config()) -> {atom(), string()}.
static_trails3_constructor(_Config) ->
  StaticRoute =
    [
      {'_',
        [
          trails:trail("/", cowboy_static, {private_file, "index.html"})
        ]}
    ],
  StaticRouteCowboy = get_static_route(),
  [{_SingleHost, StaticPath}] = StaticRoute,
  ExpectedResponse = cowboy_router:compile(StaticRouteCowboy),
  ExpectedResponse = trails:single_host_compile(StaticPath),
  {comment, ""}.

basic_trails4_constructor(_Config) ->
  BasicRouteTrails =
    [
      {'_',
        [
          trails:trail("/such/path", http_basic_route, [], #{}),
          trails:trail("/very", http_very, [], #{}),
          trails:trail("/", http_handler, [], #{})
       ]}
    ],
  BasicRouteCowboy = get_basic_route(),
  ExpectedResponse = cowboy_router:compile(BasicRouteCowboy),
  ExpectedResponse = trails:compile(BasicRouteTrails),
  {comment, ""}.

-spec static_trails4_constructor(config()) -> {atom(), string()}.
static_trails4_constructor(_Config) ->
  StaticRouteTrails =
    [
      {'_',
        [
          trails:trail("/"
                      , cowboy_static
                      , {private_file, "index.html"}
                      , #{}
                      , [])
        ]}
    ],
  StaticRouteCowboy = get_static_route(),
  [{_SingleHost, StaticPath}] = StaticRouteTrails,
  ExpectedResponse = cowboy_router:compile(StaticRouteCowboy),
  ExpectedResponse = trails:single_host_compile(StaticPath),
  {comment, ""}.

 -spec basic_metadata(config()) -> {atom(), string()}.
basic_metadata(_Config) ->
 Metadata = #{ option => 1, description => "Basic Metadata"},
  Trail =
    trails:trail("/"
                , cowboy_static
                , {private_file, "index1.html"}
                , Metadata
                , []),
  Metadata = trails:metadata(Trail),
  {comment, ""}.

 -spec put_metadata(config()) -> {atom(), string()}.
put_metadata(_Config) ->
  Metadata = #{ put => #{ description => "Put method"}},
  Trail =
    trails:trail("/"
                , cowboy_static
                , {private_file, "index2.html"}
                , Metadata
                , []),
  Metadata = trails:metadata(Trail),
  {comment, ""}.

-spec post_metadata(config()) -> {atom(), string()}.
post_metadata(_Config) ->
  Metadata = #{ post => #{ description => "Post method"}},
  Trail =
    trails:trail("/"
                , cowboy_static
                , {private_file, "index3.html"}
                , Metadata
                , []),
  Metadata = trails:metadata(Trail),
  {comment, ""}.

get_static_route() ->
    [
      {'_',
        [
          {"/", cowboy_static, {private_file, "index.html"}}
        ]}
    ].

get_basic_route() ->
    [
      {'_',
        [
          {"/such/path", http_basic_route, []},
          {"/very", http_very, []},
          {"/", http_handler, []}
       ]}
    ].

-spec basic_trails_routes(config()) -> {atom(), string()}.
basic_trails_routes(_Config) ->
  StaticRoutes =
    [ {"/", cowboy_static, {file, "www/index.html"}}
    , {"/favicon.ico", cowboy_static, {file, "www/assets/favicon.ico"}}
    , {"/assets/[...]", cowboy_static, {dir, "www/assets"}}
    , {"/game/:game_id", cowboy_static, {file, "www/game.html"}}
    ],
  ExpectedResponse1 = StaticRoutes ++
    [ {"/api/resource1/[:id]", trails_test_handler, []}
    , {"/api/:id/resource2", trails_test_handler, [arg0]}
    , {"/api/resource3/[:id]", trails_test2_handler, []}
    , {"/api/:id/resource4", trails_test2_handler, [arg0]}
    ],
  ExpectedResponse2 = StaticRoutes ++
    [ {"/api/resource1/[:id]", trails_test_handler, []}
    , {"/api/:id/resource2", trails_test_handler, [arg0]}
    ],
  ExpectedResponse3 = StaticRoutes ++
    [ {"/api/resource3/[:id]", trails_test2_handler, []}
    , {"/api/:id/resource4", trails_test2_handler, [arg0]}
    , {"/api/resource1/[:id]", trails_test_handler, []}
    , {"/api/:id/resource2", trails_test_handler, [arg0]}
    ],
  Handlers1 = [trails_test_handler, trails_test2_handler],
  Handlers2 = [trails_test2_handler, trails_test_handler],
  Trails1 = StaticRoutes ++ trails:trails(Handlers1),
  ExpectedResponse1 = Trails1,
  Trails2 = StaticRoutes ++ trails:trails(trails_test_handler),
  ExpectedResponse2 = Trails2,
  Trails3 = StaticRoutes ++ trails:trails(Handlers2),
  ExpectedResponse3 = Trails3,
  {comment, ""}.

-spec trails_store(config()) -> {atom(), string()}.
trails_store(_Config) ->
  TrailsRaw = [
    {"/resource/[:id]", trails_test_handler, []},
    {"/api/:id/resource", [], trails_test2_handler, [arg0]},
    trails:trail("/assets/[...]", cowboy_static, {dir, "www/assets"}),
    trails:trail("/such/path", http_basic_route, [], #{}),
    trails:trail("/very", http_very, [], #{}),
    trails:trail("/", http_handler, [])
  ],
  {not_started, trails} = (catch trails:all()),
  {not_started, trails} = (catch trails:retrieve("/")),
  ok = trails:store(TrailsRaw),
  Trails = normalize_paths(TrailsRaw),
  Length = length(Trails),
  Length = length(trails:all()),
  Trails = trails:all(),
  Trails1 = trails:retrieve("/assets/[...]"),
  "/assets/[...]" = trails:path_match(Trails1),
  Trails2 = trails:retrieve("/such/path"),
  "/such/path" = trails:path_match(Trails2),
  Trails3 = trails:retrieve("/very"),
  "/very" = trails:path_match(Trails3),
  Trails4 = trails:retrieve("/"),
  "/" = trails:path_match(Trails4),
  Trails5 = trails:retrieve("/resource/[:id]"),
  "/resource/[:id]" = trails:path_match(Trails5),
  Trails6 = trails:retrieve("/api/:id/resource"),
  "/api/:id/resource" = trails:path_match(Trails6),
  notfound = trails:retrieve("/other"),
  {comment, ""}.

-spec trails_api_root(config()) -> {comment, string()}.
trails_api_root(_Config) ->
  ok = trails:api_root("/api"),
  "/api" = trails:api_root(),
  Routes = [trails:trail("/things", the_handler)],
  ok = trails:single_host_compile(Routes),
  {comment, ""}.


-spec minimal_multiple_host_compile_test(config()) -> {comment, string()}.
minimal_multiple_host_compile_test(_Config) ->
  Trails1 = get_trails1(),
  [Trail1, _, Repeated1] = Trails1,
  Trails2 = get_trails2(),
  % Checks the ability to store same routes for different hosts
  ok = trails:store([{"host1", Trails1}, {"host2", Trails2}]),
  Trails1 = trails:all("host1"),
  Trails2 = trails:all("host2"),
  % trails:retrieve/1 will throw an exception if the same trail
  % is defined in more than one host or server.
  ok = try trails:retrieve("/repeated")
       catch
         throw:multiple_trails -> ok
       end,
  Trail1 = trails:retrieve("host1", "/path1"),
  Repeated1 = trails:retrieve("host1", "/repeated"),
  notfound = trails:retrieve("host2", "/path1"),
  notfound = trails:retrieve("unknown_host", "/path1"),
  % Test that trails:do_store/3 actually starts trails application
  ok = application:stop(trails),
  ok = trails:store([{"host3", Trails1}, {"host4", Trails2}]),
  {comment, ""}.

-spec minimal_multiple_server_test(config()) -> {comment, string()}.
minimal_multiple_server_test(_Config) ->
  Trails1 = get_trails1(),
  [Trail1, _, Repeated1] = Trails1,
  Trails2 = get_trails2(),
  [_, _, Repeated2] = Trails2,
  ok = trails:store(server1, [{"host1", Trails1}, {"host2", Trails2}]),
  ok = trails:store(server2, Trails2),
  ok = trails:store(server3, [{"host1", Trails1}]),
  Trails1 = trails:all(server1, "host1"),
  Trails2 = trails:all(server1, "host2"),
  Trails2 = trails:all(server2, '_'),
  ok = try trails:all("host1")
       catch
         throw:multiple_servers -> ok
       end,
  ok = try trails:retrieve("host1", "/path1")
       catch
         throw:multiple_trails -> ok
       end,
  Trail1 = trails:retrieve(server1, "host1", "/path1"),
  Trail1 = trails:retrieve(server3, "host1", "/path1"),
  Repeated1 = trails:retrieve(server1, "host1", "/repeated"),
  Repeated2 = trails:retrieve(server2, '_', "/repeated"),
  notfound = trails:retrieve(server1, "host2", "/path1"),
  notfound = trails:retrieve(server3, "host2", "/path4"),
  notfound = trails:retrieve(unknown_server, "unknown_host", "/path1"),
  {comment, ""}.

-spec server_hostmatches(config()) -> {comment, string()}.
server_hostmatches(_Config) ->
  Trails1 = get_trails1(),
  Trails2 = get_trails2(),
  Routes1 = [{"hostmatch1", Trails1}, {"hostmatch2", Trails2}],
  Dispatch1 = trails:compile(Routes1),

  Trails3 = get_trails3(),
  Routes2 = [{"hostmatch3", Trails3}],
  Dispatch2 =  trails:compile(Routes2),

  ListenerCount = 10,

  RanchOptions1 = [{port, 8080}],
  CowboyOptions1 = make_cowboy_options(Dispatch1),
  {ok, _} =
    cowboy:start_clear(server1, ListenerCount, RanchOptions1, CowboyOptions1),

  RanchOptions2 = [{port, 8081}],
  CowboyOptions2 = make_cowboy_options(Dispatch2),
  {ok, _} =
    cowboy:start_clear(server2, ListenerCount, RanchOptions2, CowboyOptions2),

  Servers = trails:servers(),
  true = lists:member(server1, Servers) andalso lists:member(server2, Servers),

  HostMatches1 = trails:host_matches(server1),
  true = lists:member(<<"hostmatch1">>, HostMatches1) andalso
    lists:member(<<"hostmatch2">>, HostMatches1),

  [<<"hostmatch3">>] = trails:host_matches(server2),

  ok = cowboy:stop_listener(server1),
  ok = cowboy:stop_listener(server2),

  {comment, ""}.

%% @private
normalize_paths(RoutesPaths) ->
  [normalize_path(Path) || Path <- RoutesPaths].

%% @private
normalize_path({PathMatch, ModuleHandler, Options}) ->
  trails:trail(PathMatch, ModuleHandler, Options);
normalize_path({PathMatch, Constraints, ModuleHandler, Options}) ->
  trails:trail(PathMatch, ModuleHandler, Options, #{}, Constraints);
normalize_path(Trail) -> Trail.

%% @private
-spec get_trails1() -> [trails:trail()].
get_trails1() ->
  [
    trails:trail("/path1", path1_handler),
    trails:trail("/path2", path2_handler),
    trails:trail("/repeated", repeated_handler)
  ].

%% @private
-spec get_trails2() -> [trails:trail()].
get_trails2() ->
  [
    trails:trail("/path3", path3_handler),
    trails:trail("/path4", path4_handler),
    trails:trail("/repeated", repeated_handler)
  ].

%% @private
-spec get_trails3() -> [trails:trail()].
get_trails3() ->
  [
    trails:trail("/path5", path5_handler),
    trails:trail("/path6", path6_handler)
  ].

%% @private
-spec make_cowboy_options(cowboy_router:dispatch_rules()) -> map().
make_cowboy_options(Dispatch) ->
  #{env =>
    #{dispatch => Dispatch},
   compress => true,
   timeout => 12000}
  .
