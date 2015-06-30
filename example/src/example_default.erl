-module(example_default).

-export(
   [
    init/3,
    rest_init/2,
    content_types_accepted/2,
    content_types_provided/2,
    forbidden/2,
    is_authorized/2,
    resource_exists/2
   ]
  ).

%% cowboy
init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {ok, Req, #{}}.

content_types_accepted(Req, State) ->
    {Method, Req1} = cowboy_req:method(Req),

    HandleMethod = case Method of
                       <<"PUT">> ->
                           handle_put;
                       <<"POST">> ->
                           handle_post;
                       <<"PATCH">> ->
                           handle_patch
                   end,

    HandleMultipartMethod = case Method of
                                <<"PUT">> ->
                                    handle_multipart_put;
                                <<"POST">> ->
                                    handle_multipart_post;
                                <<"PATCH">> ->
                                    handle_multipart_patch
                            end,
    {[
      {{<<"application">>, <<"json">>, '*'}, HandleMethod},
      {<<"application/x-www-form-urlencoded">>, HandleMethod},
      {{<<"multipart">>, <<"form-data">>, '*'}, HandleMultipartMethod}
     ],
     Req1, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, handle_get}], Req, State}.

forbidden(Req, State) ->
    {false, Req, State}.

is_authorized(Req, State) ->
    case example_auth:check_auth(Req) of
        {authenticated, User, _Req1} ->
            {true, Req, #{user => User}};
        {not_authenticated, AuthHeader, Req1} ->
            {{false, AuthHeader}, Req1, State}
    end.

resource_exists(Req, State) ->
    {Method, Req1} = cowboy_req:method(Req),
    {Method =/= <<"POST">>, Req1, State}.
