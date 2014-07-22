-module(folsomline_worker).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2,
          handle_info/2, terminate/2, code_change/3]).

-record(ctx, {file}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, #ctx{}}.

handle_call(_, _, Ctx) ->
  {reply, ok, Ctx}.

handle_cast(_, Ctx) ->
  {noreply, Ctx}.

handle_info(_, Ctx) ->
  {noreply, Ctx}.

terminate(_Reason, _Ctx) ->
  ok.

code_change(_OldVsn, Ctx, _Extra) ->
  {ok, Ctx}.
