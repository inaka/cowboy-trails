# Cowboy Trails Basic Example

To try this example, you need `git` and `Erlang` in your PATH.

```shell
$ rebar3 compile
```

To start the example server in a interactive way do this:

```shell
$ rebar3 shell
```

## Handlers' Description

If you run this `curl` command, you will see all trails' description of this server.

```shell
$ curl -i http://localhost:8080/description
[#{constraints => [],handler => example_poor_kv_handler,
   metadata =>
       #{delete =>
             #{'content-type' => "text/plain",
               desc => "Unsets an env var in the server"},
         get =>
             #{'content-type' => "text/plain",
               desc => "Gets en env var from the server"},
         put =>
             #{'content-type' => "text/plain",
               desc => "Sets an env var in the server"}},
   options => [],path_match => "/poor-kv/:key/[:value]"},
 #{constraints => [],handler => example_description_handler,
   metadata => #{get => #{desc => "Retrives trails's server description"}},
   options => [],path_match => <<"/description">>}]
```

## Poor Man's Key-Value Store

There is an additional endpoint that implements a really simple key-value store
that uses the application's env. variable as its backend. It is possible to `PUT`,
`GET` and `DELETE` values in the following way:

```shell
$ curl -XPUT "localhost:8080/poor-kv/some-key/some-value" -H "content-type: text/plain"
some-value

$ curl "localhost:8080/poor-kv/some-key" -H "content-type: text/plain"
some-value

$ curl -v "localhost:8080/poor-kv/non-existing-key" -H "content-type: text/plain"
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
