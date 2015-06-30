-module(example_echo_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {example_default,
         [
          init/3,
          rest_init/2,
          content_types_accepted/2,
          content_types_provided/2,
          resource_exists/2
         ]}
       ]).

-export(
  [
   allowed_methods/2,
   handle_put/2,
   handle_get/2
  ]).

%% cowboy
allowed_methods(Req, State) ->
  {[<<"GET">>, <<"PUT">>], Req, State}.

%% internal
handle_get(Req, State) ->
  Echo = application:get_env(example, echo, ""),
  Body = [<<"You Get a echo! ">> , Echo ],
  {Body, Req, State}.

handle_put(Req, State) ->
  {Echo, Req1} = cowboy_req:binding(echo, Req, ""),
  application:set_env(example, echo, Echo),
  Body = [<<"You put a echo! ">> , Echo ],
  Req2 = cowboy_req:set_resp_body(Body, Req1),
  {true, Req2, State}.
