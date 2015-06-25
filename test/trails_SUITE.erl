-module(trails_SUITE).
-author('elbrujohalcon@inaka.net').

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([basic_compile_test/1]).
-export([minimal_compile_test/1]).
-export([static_compile_test/1]).

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

