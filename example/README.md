# Cowboy Trails Example

To try this example, you need `git` and `Erlang` in your PATH.

```erlang

  rebar3 compile
```

To start the example server in a interactive way do this:
```erlang

  rebar3 shell
```

## Handlers Description

if you run this `curl` command, you will see all trails description of this server.

```
curl -i http://localhost:8080/description

[#{constraints => [],
   handler => cowboy_static,
   metadata => #{get => #{desc => "Static Data"}},
   options => {priv_dir,example,[],[{mimetypes,cow_mimetypes,all}]},
   path_match => <<"/[...]">>},
 #{constraints => [],
   handler => example_echo_handler,
   metadata => #{get => #{desc => "Gets echo var in the server"},
     put => #{desc => "Sets echo var in the server"}},
   options => [],
   path_match => <<"/message/[:echo]">>},
 #{constraints => [],
   handler => example_description_handler,
   metadata => #{get => #{desc => "Retrives trails's server description"}},
   options => [],
   path_match => <<"/description">>}]
```

## Poor-Man's Key-Value Store

There is an  additional endpoint that implement a really simple key-value store
that uses the application's env variable as its backend. It is possible to `PUT`,
`GET` and `DELETE` values in the following way:

```
$ curl -XPUT "localhost:8080/poor-kv/some-key/some-value" -H "content-type: text/plain"

some-value

$ curl -XGET "localhost:8080/poor-kv/some-key" -H "content-type: text/plain"

some-value

$ curl -v -XGET "localhost:8080/poor-kv/non-existing-key" -H "content-type: text/plain"

...
< HTTP/1.1 404 Not Found
...

$ curl -v -XDELETE "localhost:8080/poor-kv/some-key" -H "content-type: text/plain"

...
< HTTP/1.1 204 No Content
...

$ curl -v -XDELETE "localhost:8080/poor-kv/some-key" -H "content-type: text/plain"

...
< HTTP/1.1 404 Not Found
...
```
