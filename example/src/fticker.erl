-module(fticker).

%% Don't do that at home!
-behaviour(application).
-behaviour(supervisor).
-behaviour(gen_server).

-define(SUPER, fticker_sup).
-define(SERVER, fticker_server).

-export([simple/0, show/0]).
-export([start/0, start/2, stop/0, stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2,
          handle_info/2, terminate/2, code_change/3]).

-record(ctx, {file}).

%% api

simple() ->
  {ok, Metrics} = folsomline:read(),
  [transform(R) || R <- Metrics].

show() ->
  list_as_table:print(simple()).

%% shell's
start() ->
  {ok, _} = application:ensure_all_started(?MODULE).

%% shell's
stop() ->
  ok = application:stop(?MODULE).

%% app's
start(_Type, _StartArgs) ->
  supervisor:start_link({local, ?SUPER}, ?MODULE, []).

%% app's
stop(_State) ->
  ok.

%% gen_servers's
start_link(Args) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%% callbacks

%% genserv init
init([worker]) ->
  folsom_metrics:new_counter(counter),
  folsom_metrics:new_gauge(gauge),
  folsom_metrics:new_meter(meter),
  {ok, #ctx{}, 0};
%% supervisor's init
init([]) ->
  % strategy
  MaxRestart = 3,
  MaxWait = 3600,
  RestartStrategy = {one_for_one, MaxRestart, MaxWait},
  Children = [{worker, {?MODULE, start_link, [[worker]]},
                    permanent, 2000, worker, [?MODULE]}],
  {ok, {RestartStrategy, Children}}.

handle_call(_, _, Ctx) ->
  {stop, unknown_command, Ctx}.

handle_cast(_, Ctx) ->
  {stop, unknown_command, Ctx}.

handle_info(timeout, Ctx) ->
  count(),
  gauge(),
  meter(),
  Timeout = crypto:rand_uniform(0,1000),
  {noreply, Ctx, Timeout}.

terminate(_Reason, _Ctx) ->
  ok.

code_change(_OldVsn, Ctx, _Extra) ->
  {ok, Ctx}.

%% priv

count() ->
  Count = crypto:rand_uniform(1,11),
  case crypto:rand_uniform(0,2) of
    N when N >= 1 ->
      folsom_metrics:notify({counter, {inc, Count}});
    _ ->
      folsom_metrics:notify({counter, {dec, Count}})
  end.

gauge() ->
  V = crypto:rand_uniform(1,105),
  folsom_metrics:notify({gauge, V}).

meter() ->
  V = crypto:rand_uniform(1,11),
  folsom_metrics:notify({meter, V}).

transform(R) ->
  transform(R, []).

transform([], Acc) -> lists:reverse(Acc);
transform([{time, {H,M,S}}|T], Acc) ->
  R = [{time, lists:flatten(io_lib:fwrite("~2..0b:~2..0b:~2..0b", [H,M,S]))}],
  transform(T, R ++ Acc);
transform([{meter, M}|T], Acc) ->
  {one, One} = lists:keyfind(one, 1, M),
  {five, Five} = lists:keyfind(five, 1, M),
  {fifteen, Fifteen} = lists:keyfind(fifteen, 1, M),
  R = [{fifteen, lists:flatten(io_lib:fwrite("~.3f", [Fifteen*1.0]))},
    {five, lists:flatten(io_lib:fwrite("~.3f", [Five*1.0]))},
    {one, lists:flatten(io_lib:fwrite("~.3f", [One*1.0]))},
    lists:keyfind(count, 1, M)],
  transform(T, R ++ Acc);
transform([R|T], Acc) ->
  transform(T, [R|Acc]).
