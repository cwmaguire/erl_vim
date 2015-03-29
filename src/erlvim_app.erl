-module(erlvim_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-define(ANY_HOST, '_').
-define(NO_OPTIONS, []).

start(_Type, _Args) ->

    Paths = [{"/", erlvim_websocket, ?NO_OPTIONS},
             {"/[...]", cowboy_static, {priv_dir, erlvim, "static"}}],
    Routes = [{?ANY_HOST, Paths}],
    Dispatch = cowboy_router:compile(Routes),
    _ = cowboy:start_http(erlvim_http_listener, 100, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]),
    erlvim_sup:start_link().

stop(_State) ->
	ok.
