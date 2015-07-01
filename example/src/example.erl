-module(example).

-export([start/0]).
-export([start/2]).
-export([stop/0]).
-export([stop/1]).
-export([start_phase/3]).

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

  StaticTrail =
    trails:trail(<<"/[...]">>
                , cowboy_static
                , {priv_dir
                  , example
                  , ""
                  , [{mimetypes, cow_mimetypes, all}]}
                ,  #{get => #{desc => "Static Files"}}),
  Trails =
     trails:trails(example_echo_handler) ++ [ DescriptionTrail, StaticTrail],
  trails:store(Trails),
  Dispatch = trails:single_host_compile(Trails),
  RanchOptions = [{port, Port}],
  CowboyOptions =
      [
       {env,
        [
         {dispatch, Dispatch}
        ]},
       {compress, true},
       {timeout, 12000}
      ],
  {ok, _} =
    cowboy:start_http(example_http, ListenerCount, RanchOptions, CowboyOptions),
  ok.
