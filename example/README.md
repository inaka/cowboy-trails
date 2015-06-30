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

Eco example
You can set a response on the server with PUT http://localhost:8080/message/echo
```
curl -v -H "Content-Type: text/plain" -X PUT http://localhost:8080/message/yahooooo!!!
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Tue, 30 Jun 2015 18:48:00 GMT
content-length: 22
content-type: application/json

Got a eco! yahooooo!!!⏎
```

Description example
if you run this curl command, you will see all trails description of this server
```
curl -i http://localhost:8080/description
```
