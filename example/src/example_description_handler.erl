-module(example_description_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {example_default,
         [
          init/2,
          content_types_accepted/2,
          content_types_provided/2,
          resource_exists/2
         ]}
       ]).

-export(
  [
   allowed_methods/2,
   handle_get/2
  ]).

%% cowboy
allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

%% internal
handle_get(Req, State) ->
  Body = trails:all(),
  {io_lib:format("~p~n", [Body]), Req, State}.
