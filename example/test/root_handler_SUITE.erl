-module(root_handler_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

all() ->
    [
     application_should_be_started
    ].

init_per_suite(Config) ->
    ibrowse:start(),
    example:start(),
    {ok, Port} = application:get_env(example, http_port),
    Url = "http://localhost:" ++ integer_to_list(Port),
    [ {url, Url} | Config ].

application_should_be_started(Config) ->
    Headers = [{content_type, "application/json"}, {basic_auth, {"test", "test"}}],
    {ok, StatusCode, _ResponseHeaders , _Body} = ibrowse:send_req(?config(url, Config), Headers, get),
    "200" = StatusCode.

end_per_suite(_Config) ->
    ibrowse:stop(),
    example:stop().
