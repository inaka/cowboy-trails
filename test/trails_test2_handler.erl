-module(trails_test2_handler).

-behaviour(trails_handler).

%% API
-export([trails/0]).

trails() ->
    [{"/api/resource3/[:id]", trails_test2_handler, []},
     {"/api/:id/resource4", trails_test2_handler, [arg0]}].
