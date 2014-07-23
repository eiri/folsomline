%% @doc Folsomline - missing persistence for folsom.
%% @version 0.1
%% @reference <a href="https://github.com/eiri/folsomline">https://github.com/eiri/folsomline</a>
%% @author Eric Avdey <eiri@eiri.ca>
%% @copyright 2014 Eric Avdey

-module(folsomline).

-author('Eric Avdey <eiri@eiri.ca>').

-behaviour(application).
-behaviour(supervisor).

-define(WORKER(M), {M, {M, start_link, []}, permanent, 2000, worker, [M]}).

%% pub API
-export([read/0]).

%% behaviours callbacks
-export([start/0, stop/0, start/2, stop/1, init/1]).

%%
%% API
%%

%% @doc Reads all stored and timestamped metrics
-spec read() -> {ok, Metrics::list()} | {error, Error::term()}.
read() ->
  folsomline_worker:read().

%% app/sup start/stop

start() ->
  {ok, _} = application:ensure_all_started(?MODULE).

stop() ->
  ok = application:stop(?MODULE).

start(_Type, _StartArgs) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
  ok.

init([]) ->
  % strategy
  MaxRestart = 3,
  MaxWait = 3600,
  RestartStrategy = {one_for_one, MaxRestart, MaxWait},
  Children = [?WORKER(folsomline_worker)],
  {ok, {RestartStrategy, Children}}.
