-module(example_poor_kv_handler).

-include_lib("mixer/include/mixer.hrl").

-mixin([{example_default,
         [init/2, content_types_accepted/2, content_types_provided/2, resource_exists/2]}]).

-export([allowed_methods/2, handle_put/2, handle_get/2, delete_resource/2]).

%trails
-behaviour(trails_handler).

-export([trails/0]).

trails() ->
    MsgTrailsMetadata =
        #{get => #{desc => "Gets en env var from the server", 'content-type' => "text/plain"},
          put => #{desc => "Sets an env var in the server", 'content-type' => "text/plain"},
          delete => #{desc => "Unsets an env var in the server", 'content-type' => "text/plain"}},
    [trails:trail("/poor-kv/:key/[:value]", ?MODULE, [], MsgTrailsMetadata)].

%% cowboy
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>, <<"DELETE">>, <<"HEAD">>], Req, State}.

%% internal
handle_get(Req, State) ->
    Key = cowboy_req:binding(key, Req),
    case application:get_env(example, Key, undefined) of
        undefined ->
            {halt, cowboy_req:reply(404, Req), State};
        Value ->
            {Value, Req, State}
    end.

handle_put(Req, State) ->
    Key = cowboy_req:binding(key, Req),
    case cowboy_req:binding(value, Req, undefined) of
        undefined ->
            {false, Req, State};
        Value ->
            application:set_env(example, Key, Value),
            {true, cowboy_req:set_resp_body(Value, Req), State}
    end.

delete_resource(Req, State) ->
    Key = cowboy_req:binding(key, Req),
    application:unset_env(example, Key),
    {true, Req, State}.
