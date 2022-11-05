-module(docker_ex_http_svr).

% -compile({parse_transform, leptus_pt}).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-record(state, {op}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0]).
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, init/2, allowed_methods/2, content_types_provided/2, handle_get/2,
  handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
  io:fwrite("Starting docker_ex_http_svr !~n", []),
  application:ensure_all_started(cowboy),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(Args) ->
  Dispatch = cowboy_router:compile([{'_', [
    {"/", ?MODULE, []},
    {"/dtplant", dtplant,[]}
  ]}]),
  {ok, _} =
    cowboy:start_clear(my_http_listener, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
  {ok, Args}.

init(Req, State) ->
  io:fwrite("req data !~p ~n", [1111]),
  {cowboy_rest, Req, State}.


allowed_methods(Req, State) ->
  Methods = [<<"GET">>, <<"DELETE">>],
  {Methods, Req, State}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, handle_get}], Req, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
handle_get(Req, State) ->
 Body = "Hello Erlang ...",
  {Body, Req, State}.
