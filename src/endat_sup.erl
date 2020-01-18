%%%-------------------------------------------------------------------
%%% Created : 3 Jan 2020 by Thiago Esteves <thiagocalori@gmail.com>
%%%
%%% @doc
%%% This is the ENDAT top level supervisor where the user can create
%%% or remove ENDAT gen-servers.
%%% @end
%%%-------------------------------------------------------------------

-module(endat_sup).

-behaviour(supervisor).

-author('Thiago Esteves').

%%====================================================================
%% API functions
%%====================================================================

-export([start_link/0]).

-export([init/1, create_endat/1, remove_endat/1]).

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(ENDAT_DRIVER_NAME, endat_driver).
-define(ENDAT_MODULE_NAME, endat).
-define(ENDAT_TIMEOUT, 5000). % ms

%%====================================================================
%% API functions implementation
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 4,
                 period => 30},
    %% Endat driver is linked at the initialisation
    ChildSpecs = [#{id => ?ENDAT_DRIVER_NAME,
                    start => {?ENDAT_DRIVER_NAME, start_link, []},
                    shutdown => brutal_kill}],
    % comment this line to stop trapping exits
    process_flag(trap_exit, true),
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================

create_endat(Instance) ->
  EndatName = compose_endat_name(Instance),
  EndatSpec = {EndatName, { ?ENDAT_MODULE_NAME, start_link, [[EndatName, Instance]]},
        permanent, ?ENDAT_TIMEOUT, worker, [?ENDAT_MODULE_NAME]},
  supervisor:start_child(?MODULE, EndatSpec).

remove_endat(Instance) ->
  EndatName = compose_endat_name(Instance),
  supervisor:terminate_child(?MODULE, EndatName),
  supervisor:delete_child(?MODULE, EndatName).

%%====================================================================
%% Internal functions
%%====================================================================

i2l(I) -> erlang:integer_to_list(I).
l2a(L) -> erlang:list_to_atom(L).

%% compose gen server name
compose_endat_name(Id) ->
  Name = "Endat:"++i2l(Id),
  l2a(Name).
