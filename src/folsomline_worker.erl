-module(folsomline_worker).

-behaviour(gen_server).

-export([start_link/0, read/0]).
-export([init/1, handle_call/3, handle_cast/2,
          handle_info/2, terminate/2, code_change/3]).

-record(ctx, {tref, file}).
-define(LOG, folsom).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

read() ->
  read(start, []).

init([]) ->
  {ok, File} = application:get_env(folsomline, dbfile),
  case disk_log:open([{name, ?LOG}, {file, File}]) of
    {ok, ?LOG} ->
      lager:debug("Opened ~s", [File]);
    {repaired,?LOG,_,_} ->
      lager:debug("Repaired ~s", [File])
  end,
  process_flag(trap_exit, true),
  {_,_,S} = erlang:time(),
  lager:debug("Starting clock in ~b sec.", [60 - S]),
  {ok, TRef} = timer:send_after(timer:seconds(60 - S), start),
  {ok, #ctx{tref= TRef, file = File}}.

handle_call(_, _, Ctx) ->
  {stop, unknown_call, Ctx}.

handle_cast(_, Ctx) ->
  {stop, unknown_call, Ctx}.

handle_info(store, Ctx) ->
  Time = erlang:time(),
  Keys = folsom_metrics:get_metrics(),
  Data = [{K, folsom_metrics:get_metric_value(K)} || K <- Keys],
  disk_log:log(?LOG, [{time, Time}|Data]),
  {noreply, Ctx};
handle_info(start, Ctx) ->
  lager:debug("Clock started"),
  {ok, Interval} = application:get_env(folsomline, interval),
  {ok, TRef} = timer:send_interval(Interval, store),
  {noreply, Ctx#ctx{tref = TRef}};
handle_info({'EXIT', _, normal}, Ctx) ->
  {noreply, Ctx}.

terminate(_Reason, _Ctx) ->
  disk_log:close(?LOG).

code_change(_OldVsn, Ctx, _Extra) ->
  {ok, Ctx}.

%% priv

read(Cont, Acc) ->
  case disk_log:chunk(?LOG, Cont) of
    eof ->
      {ok, Acc};
    {error, Error} ->
      {error, Error};
    {Cont2, T} ->
      read(Cont2, T ++ Acc);
    {Cont2, T, _} ->
      read(Cont2, T ++ Acc)
  end.
