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
  {ok, _Pid} = start_listeners(),
  {ok, Pid}.

%% @private
stop(_State) ->
  ok = cowboy:stop_listener(example_http).

start_listeners() ->
  {ok, Port} = application:get_env(example, http_port),
  {ok, ListenerCount} = application:get_env(example, http_listener_count),
  DescriptionTrail =
    trails:trail(<<"/description">>
                , example_description_handler
                , []
                , #{get => #{desc => "Retrives trails's server description"}}),
  MessageTrail =
  trails:trail(<<"/message/[:echo]">>
              , example_echo_handler
              , []
              ,  #{ get => #{desc => "Gets echo var in the server"}
                  , put => #{desc => "Sets echo var in the server"}}),
  StaticTrail =
    trails:trail(<<"/[...]">>
                , cowboy_static
                , {priv_dir
                  , example
                  , ""
                  , [{mimetypes, cow_mimetypes, all}]}
                ,  #{get => #{desc => "Static Data"}}),
  Trails = [DescriptionTrail, MessageTrail, StaticTrail],
  trails:store(Trails),
  Dispatch = trails:single_host_compile(Trails),

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
