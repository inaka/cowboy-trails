-module(example).

-export([start/0]).
-export([start/2]).
-export([stop/0]).
-export([stop/1]).
-export([start_phase/3]).

-behaviour(application).
-hank([{unnecessary_function_arguments, [{start_phase, 3}]}]).

%% application
%% @doc Starts the application
start() ->
  application:ensure_all_started(example).

%% @doc Stops the application
stop() ->
  application:stop(example).

%% behaviour
%% @private
start(_StartType, _StartArgs) ->
  example_sup:start_link().

%% @private
stop(_State) ->
  ok = cowboy:stop_listener(example_http).

% start_listeners() ->
-spec start_phase(atom(), application:start_type(), []) -> ok | {error, term()}.
start_phase(start_trails_http, _StartType, []) ->
  {ok, Port} = application:get_env(example, http_port),
  {ok, ListenerCount} = application:get_env(example, http_listener_count),
  DescriptionTrail =
    trails:trail(<<"/description">>
                , example_description_handler
                , []
                , #{get => #{desc => "Retrives trails's server description"}}),
  Handlers = [example_poor_kv_handler],
  Trails = trails:trails(Handlers) ++ [DescriptionTrail],
  trails:store(Trails),
  Dispatch = trails:single_host_compile(Trails),
  CowboyOptions = #{ env => #{ dispatch => Dispatch
                             }
                   , request_timeout => 12000
                   },
  {ok, _} =
    cowboy:start_clear(example_http,
                       #{ socket_opts => [ {port, Port}
                                         ]
                        , num_acceptors => ListenerCount
                        },
                       CowboyOptions),
  ok.
