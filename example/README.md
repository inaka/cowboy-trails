# Cowboy Trails Example
===

To try this example, you need GNU `make` , `git` and `Erlang` in your PATH.

```
  make deps
```

To start the example server in a interactive way do this:
```
  make app shell
```

Example outputs
---------------

If you put this url in your browser, you will see a beatiful picture :).
```
http://localhost:8080/index.html
```

Echo example
You can set a response on the server with PUT http://localhost:8080/message/echo
```
curl -v -H "Content-Type: text/plain" -X PUT http://localhost:8080/message/yahooooo!!!
< HTTP/1.1 200 OK
< connection: keep-alive
* Server Cowboy is not blacklisted
< server: Cowboy
< date: Wed, 01 Jul 2015 18:27:22 GMT
< content-length: 25
< content-type: text/plain
<
* Connection #0 to host localhost left intact
You put a echo! yahooooo!⏎
```

Description example
if you run this curl command, you will see all trails description of this server
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
