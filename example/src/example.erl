-module(example).

-export([start/0]).
-export([start/2]).
-export([stop/0]).
-export([stop/1]).

%% application
%% @doc Starts the application
start() ->
  {ok, _Started} = application:ensure_all_started(example).

%% @doc Stops the application
stop() ->
  application:stop(example).

%% behaviour
%% @private
start(_StartType, _StartArgs) ->
  {ok, Pid} = example_sup:start_link(),
  start_listeners(),
  {ok, Pid}.

%% @private
stop(_State) ->
  cowboy:stop_listener(example_http),
  ok.

start_listeners() ->
  {ok, Port} = application:get_env(example, http_port),
  {ok, ListenerCount} = application:get_env(example, http_listener_count),
  Trails =
    [
      trails:trail(<<"/">>, example_root_handler, [],  #{get => #{}})
     ,trails:trail(<<"/description">>, example_description_handler, [],  #{get => #{}})
     ,trails:trail(<<"/message/[:eco]">>, example_eco_handler, [],  #{get => #{}, put => #{}})
     ,trails:trail(<<"/[...]">>, cowboy_static, {priv_dir, example, "",[{mimetypes, cow_mimetypes, all}]},  #{get => #{}})
    ],
  trails:store(Trails),
  Dispatch =  trails:compile( [{'_', Trails}]),

  RanchOptions = [{port, Port}  ],
  CowboyOptions =
      [
       {env,
        [
         {dispatch, Dispatch}
        ]},
       {compress, true},
       {timeout, 12000}
      ],
  cowboy:start_http(example_http, ListenerCount, RanchOptions, CowboyOptions).
