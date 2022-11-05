%%%-------------------------------------------------------------------
%% @doc docker_ex public API
%% @end
%%%-------------------------------------------------------------------

-module(docker_ex_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    docker_ex_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
