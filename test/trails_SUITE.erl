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
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec minimal_compile_test(config()) -> config().
minimal_compile_test(Config) ->
  MininalRoute = [{'_',[]}],
  ExpectedResponse = trails:compile(MininalRoute),
  Config.

-spec basic_compile_test(config()) -> config().
basic_compile_test(Config) ->
  BasicRoute =
    [
      {"localhost",
        [
          {"/such/path", http_such_path_handler, []},
          {"/very", http_very, []},
          {"/", http_handler, []}
       ]}
    ],
  ExpectedResponse = trails:compile(BasicRoute),
  Config.

-spec static_compile_test(config()) -> config().
static_compile_test(Config) ->
  StaticRoute =
    [
      {'_',
        [
          {"/", cowboy_static, {private_file, "index.html"}}
        ]}
    ],
  ExpectedResponse = trails:compile(StaticRoute),
  Config.

