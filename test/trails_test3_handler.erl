-module(trails_test3_handler).

-behaviour(trails_handler).

%% API
-export([trails/1]).

trails(Opts) ->
  [
   {"/api/resource5/[:id]", trails_test3_handler, []},
   {"/api/:id/resource6", trails_test3_handler, [Opts]}
  ].
