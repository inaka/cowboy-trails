-module(trails_SUITE).
-author('elbrujohalcon@inaka.net').

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
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
-export([basic_trails_routes/1]).


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
  application:ensure_all_started(cowboy),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec minimal_compile_test(config()) -> {atom(), string()}.
minimal_compile_test(_Config) ->
  MininalRoute = [{'_',[]}],
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
          {"/very", http_very, []},
          {"/", http_handler, []}
       ]}
    ],
  ExpectedResponse = cowboy_router:compile(BasicRoute),
  ExpectedResponse = trails:compile(BasicRoute),
  {comment, ""}.

-spec static_compile_test(config()) -> {atom(), string()}.
static_compile_test(_Config) ->
  StaticRoute =
    [
      {'_',
        [
          {"/", cowboy_static, {private_file, "index.html"}}
        ]}
    ],
  ExpectedResponse = cowboy_router:compile(StaticRoute),
  ExpectedResponse = trails:compile(StaticRoute),
  {comment, ""}.

-spec minimal_single_host_compile_test(config()) -> {atom(), string()}.
minimal_single_host_compile_test(_Config) ->
  MininalRoute = [{'_',[]}],
  [{_SingleHost, MininalPath}] = MininalRoute,
  ExpectedResponse = cowboy_router:compile(MininalRoute),
  ExpectedResponse = trails:single_host_compile(MininalPath),
  {comment, ""}.

-spec basic_single_host_compile_test(config()) -> {atom(), string()}.
basic_single_host_compile_test(_Config) ->
  BasicRoute =
    [
      {'_',
        [
          {"/such/path", http_such_path_handler, []},
          {"/very", http_very, []},
          {"/", http_handler, []}
       ]}
    ],
  ExpectedResponse = cowboy_router:compile(BasicRoute),
  [{_SingleHost, BasicPath}] = BasicRoute,
  ExpectedResponse = trails:single_host_compile(BasicPath),
  {comment, ""}.

-spec static_single_host_compile_test(config()) -> {atom(), string()}.
static_single_host_compile_test(_Config) ->
  StaticRoute =
    [
      {'_',
        [
          {"/", cowboy_static, {private_file, "index.html"}}
        ]}
    ],
  [{_SingleHost, StaticPath}] = StaticRoute,
  ExpectedResponse = cowboy_router:compile(StaticRoute),
  ExpectedResponse = trails:single_host_compile(StaticPath),
  {comment, ""}.

basic_trails2_constructor(_Config) ->
  BasicRoute =
    [
      {'_',
        [
          trails:trail("/such/trails/arity/two", trails_handler),
          trails:trail("/very", http_very),
          trails:trail("/", http_handler)
       ]}
    ],
  BasicRouteCowboy =
    [
      {'_',
        [
          {"/such/trails/arity/two", trails_handler, []},
          {"/very", http_very, []},
          {"/", http_handler, []}
       ]}
    ],
  ExpectedResponse = cowboy_router:compile(BasicRouteCowboy),
  ExpectedResponse = trails:compile(BasicRoute),
  {comment, ""}.

basic_trails3_constructor(_Config) ->
  BasicRoute =
    [
      {'_',
        [
          trails:trail("/such/trails/arity/three", trails_handler, []),
          trails:trail("/very", http_very, []),
          trails:trail("/", http_handler, [])
       ]}
    ],
  BasicRouteCowboy =
    [
      {'_',
        [
          {"/such/trails/arity/three", trails_handler, []},
          {"/very", http_very, []},
          {"/", http_handler, []}
       ]}
    ],
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
  StaticRouteCowboy =
    [
      {'_',
        [
          {"/", cowboy_static, {private_file, "index.html"}}
        ]}
    ],
  [{_SingleHost, StaticPath}] = StaticRoute,
  ExpectedResponse = cowboy_router:compile(StaticRouteCowboy),
  ExpectedResponse = trails:single_host_compile(StaticPath),
  {comment, ""}.

basic_trails4_constructor(_Config) ->
  BasicRouteTrails =
    [
      {'_',
        [
          trails:trail("/such/trails/arity/four", trails_handler, [], []),
          trails:trail("/very", http_very, [], []),
          trails:trail("/", http_handler, [], [])
       ]}
    ],
  BasicRouteCowboy =
    [
      {'_',
        [
          {"/such/trails/arity/four", trails_handler, []},
          {"/very", http_very, []},
          {"/", http_handler, []}
       ]}
    ],
  ExpectedResponse = cowboy_router:compile(BasicRouteCowboy),
  ExpectedResponse = trails:compile(BasicRouteTrails),
  {comment, ""}.

-spec static_trails4_constructor(config()) -> {atom(), string()}.
static_trails4_constructor(_Config) ->
  StaticRouteTrails =
    [
      {'_',
        [
          trails:trail("/", cowboy_static, {private_file, "index.html"},[])
        ]}
    ],
  StaticRouteCowboy =
    [
      {'_',
        [
          {"/", cowboy_static, {private_file, "index.html"}}
        ]}
    ],
  [{_SingleHost, StaticPath}] = StaticRouteTrails,
  ExpectedResponse = cowboy_router:compile(StaticRouteCowboy),
  ExpectedResponse = trails:single_host_compile(StaticPath),
  {comment, ""}.

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
