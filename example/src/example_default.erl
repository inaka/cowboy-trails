-module(example_default).

-export(
   [
    init/2,
    content_types_accepted/2,
    content_types_provided/2,
    forbidden/2,
    resource_exists/2
   ]
  ).

%% cowboy
init(Req, State) ->
  {cowboy_rest, Req, State}.

content_types_accepted(Req, State) ->
  {[{<<"text/plain">>, handle_put}], Req, State}.

content_types_provided(Req, State) ->
  {[{<<"text/plain">>, handle_get}], Req, State}.

forbidden(Req, State) ->
  {false, Req, State}.

resource_exists(Req, State) ->
  case cowboy_req:binding(key, Req) of
    undefined -> {true, Req, State};
    Key ->
      case application:get_env(example, Key, undefined) of
        undefined -> {false, Req, State};
        _ -> {true, Req, State}
      end
  end.
