-module(trails_test_handler).

-behaviour(trails_handler).

%% API
-export([trails/0]).

trails() ->
    [{"/api/resource1/[:id]", trails_test_handler, []},
     {"/api/:id/resource2", trails_test_handler, [arg0]}].
