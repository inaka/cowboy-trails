-module(trails_app).
-moduledoc false.

-behaviour(application).

-export([start/2, stop/1]).

-spec start(term(), term()) -> {error, term()} | {ok, pid()}.
start(_Type, _Args) ->
    trails_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
