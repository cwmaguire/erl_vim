-module(erlvim_mode_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [{conn_fsm,
              {erlvim_normal, start_link, []},
              transient,
              brutal_kill,
              worker,
              [erlvim_normal]}],
    {ok, {{simple_one_for_one, 1, 5}, Procs}}.
