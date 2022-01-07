<img src="https://lh5.googleusercontent.com/-Y1n1Vh4FjLE/TXDZiQ_zSVI/AAAAAAAAAJY/h47az_0MxO0/s1600/Western+backdrop+04.png" height="200" width="100%" />

# cowboy-trails

![build](https://github.com/inaka/cowboy-trails/workflows/build/badge.svg)

Cowboy routes on steroids!

## Contact Us

If you find any **bugs** or have a **problem** while using this library, please
[open an issue](https://github.com/inaka/cowboy-trails/issues/new) in this repo
(or a pull request 😄).

And you can check out all of our open-source projects at [inaka.github.io](http://inaka.github.io).

## Why Cowboy Trails?

**Cowboy-Trails** enables you to:

* add information to `cowboy` routes, which can later be used to interact with
  the server in a higher abstraction level,
* define the server routes directly within the module that implements them.

## How to Use it?

The most common use case for `cowboy_trails` is to compile `cowboy` routes.

Normally with `cowboy` you compile routes in the following way:

```erlang
Routes = [{'_',
           [ {"/resource1", resource1_handler, []}
           , {"/resource2/[:id]", resource2_handler, []}
           ]
          }
         ],
cowboy_router:compile(Routes),
```

Trails is fully compatible with `cowboy` routes, so you can pass the same
routes in order to be processed by Trails:

```erlang
trails:compile(Routes),
```

So far it seems like there's no difference, right? But most of the time,
with `cowboy`, you usually work with only a single host, but you're
required to keep defining the host parameter within the routes (`[{'_', [...]}]`).

Well, with Trails you have another useful function to compile single host routes:

```erlang
%% You only define the routes/paths
Routes = [ {"/resource1", resource1_handler, []}
         , {"/resource2/[:id]", resource2_handler, []}
         ],
trails:single_host_compile(Routes),
```

Now, let's suppose you want to add metadata to
`cowboy` routes related with the semantics of each HTTP method.

You'd do something like:

```erlang
Metadata = #{put => #{description => "PUT method"},
             post => #{description => "POST method"},
             get => #{description => "GET method"}},
Trail = trails:trail("/",
                     cowboy_static,
                     {private_file, "index2.html"},
                     Metadata,
                     []),
%% You can later retrieve the metadata:
Metadata = trails:metadata(Trail),
```

This can then be used to generate documentation related to each endpoint.

Also, when you work with `cowboy`, you have to define all routes in one place:

```erlang
Routes =
  [{'_',
    [ {"/", cowboy_static, {file, "www/index.html"}}
    , {"/favicon.ico", cowboy_static, {file, "www/assets/favicon.ico"}}
    , {"/assets/[...]", cowboy_static, {dir, "www/assets"}}
    , {"/game/:game_id", cowboy_static, {file, "www/game.html"}}
    , {"/api/status", spts_status_handler,  []}
    , {"/api/games", spts_games_handler, []}
    , {"/api/games/:game_id", spts_single_game_handler, []}
    , {"/api/games/:game_id/serpents", spts_serpents_handler, []}
    , { "/api/games/:game_id/serpents/:token"
      , spts_single_serpent_handler, []
      }
    , {"/api/games/:game_id/news", lasse_handler, [spts_news_handler]}
    ]
   }
  ],
Dispatch = cowboy_router:compile(Routes),
```

But now, with `trails`, you're able to define the routes on each of your resource handlers,
separately.
These handlers must implement callback `trails/0` or `trails/1` and return the specific
routes that define them. For a better understanding, you can check out the
examples in the `test` folder ([trails_test_handler](./test/trails_test_handler.erl)).

Once you have implemented the `trails/0` or `trails/1` callback on your handlers, you can do
something like this:

```erlang
Handlers =
  [ spts_status_handler
  , spts_games_handler
  , spts_single_game_handler
  , spts_serpents_handler
  , spts_single_serpent_handler
  , spts_news_handler
  , {support_params_handler, #{key => value}}
  ],
Trails =
  [ {"/", cowboy_static, {file, "www/index.html"}}
  , {"/favicon.ico", cowboy_static, {file, "www/assets/favicon.ico"}}
  , {"/assets/[...]", cowboy_static, {dir, "www/assets"}}
  , {"/game/:game_id", cowboy_static, {file, "www/game.html"}}
  | trails:trails(Handlers)
  ],
trails:single_host_compile(Trails),
```

This way each handler maintains their own routes, as it should be, and you can
merge them easily.

## Example

For more information about `cowboy_trails`, how to use it and the different
functions that it exposes, please check this [example](./example).
