%%%-------------------------------------------------------------------
%% @doc docker_ex top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(docker_ex_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    io:fwrite("Starting docker_ex_sup !~n", []),
    SupFlags =
        #{strategy => one_for_all,
          intensity => 0,
          period => 1},
          
    ChildSpecs =
        [{docker_ex_http_svr,
          {docker_ex_http_svr, start_link, []},
          permanent,
          10000,
          worker,
          [docker_ex_http_svr]}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
