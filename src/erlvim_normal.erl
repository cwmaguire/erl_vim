-module(erlvim_normal).
-behaviour(gen_server).

%% API.
-export([start_link/1]).
-export([handle/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {socket :: pid()}).

%% API.

-spec start_link(pid()) -> {ok, pid()}.
start_link(Socket) ->
	gen_server:start_link(?MODULE, Socket, []).

handle(Pid, KeyEvent) ->
    gen_server:cast(Pid, KeyEvent).

%% gen_server.

init(Socket) ->
	{ok, #state{socket = Socket}}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast([{<<"key">>, Key}, _Shift, _Ctrl], State = #state{socket = Socket}) ->
    io:format("Sending event to web page~n"),
    Socket ! {send, ["Received Key ", Key], self()},
	{noreply, State};
handle_cast(Msg, State) ->
    io:format("erlvim_normal: unrecognized message: ~p~n", [Msg]),
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
