-module(erlvim_text).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([curosr_move/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {text :: list(),
                cursor_view_pos :: {integer(), integer()},
                cursor_text_pos :: {integer(), integer()},
                socket :: pid()}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

cursor_move(Dir) ->
    gen_server:cast(?MODULE, {cursor_move, down}).

%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({cursor_move, Dir}, #state{socket = Socket} = State) ->
    {State2, Commands} = cursor_move(Dir),
    [Socket ! {send, Command} || Command <- Commands],
	{noreply, State2}.
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

cursor_move(Down, #state{text = Text, cursor = {X, Y}} = State) ->
    case next_line({X,Y}, Text) of
        true ->
            
